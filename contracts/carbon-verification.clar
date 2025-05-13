;; Carbon Offset Verification Platform

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))

;; Data vars
(define-data-var total-offsets uint u0)

;; Maps
(define-map user-offsets principal uint)
(define-map verified-offsets uint {
    owner: principal,
    amount: uint,
    verified: bool
})

;; Public functions
(define-public (register-offset (amount uint))
    (let
        ((user tx-sender)
         (current-id (var-get total-offsets)))
        (if (> amount u0)
            (begin
                (map-set verified-offsets current-id {
                    owner: user,
                    amount: amount,
                    verified: false
                })
                (var-set total-offsets (+ current-id u1))
                (ok current-id))
            ERR-INVALID-AMOUNT)))
;; Constants

;; Data vars
(define-data-var contract-owner principal tx-sender)

;; The verification check now correctly references the contract owner
(define-public (verify-offset (offset-id uint))
    (let ((offset (unwrap! (map-get? verified-offsets offset-id) ERR-INVALID-AMOUNT)))
        (if (is-eq tx-sender (var-get contract-owner))
            (begin
                (map-set verified-offsets offset-id (merge offset {verified: true}))
                (ok true))
            ERR-NOT-AUTHORIZED)))

;; Read-only functions
(define-read-only (get-offset (offset-id uint))
    (map-get? verified-offsets offset-id))

(define-read-only (get-total-offsets)
    (var-get total-offsets))



;; Carbon Credit Token Contract
(define-fungible-token carbon-credit)

;; Constants
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))

;; Data vars
(define-data-var token-uri (string-utf8 256) u"")

;; Public functions
(define-public (mint (amount uint) (recipient principal))
    (begin
        (ft-mint? carbon-credit amount recipient)))

(define-public (transfer (amount uint) (sender principal) (recipient principal))
    (begin
        (ft-transfer? carbon-credit amount sender recipient)))

;; Read-only functions
(define-read-only (get-balance (account principal))
    (ft-get-balance carbon-credit account))

(define-read-only (get-token-uri)
    (var-get token-uri))


;; Carbon Credit Marketplace Contract

;; Constants
(define-constant ERR-LISTING-NOT-FOUND (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))

;; Data vars
(define-data-var listing-nonce uint u0)

;; Maps
(define-map listings uint {
    seller: principal,
    amount: uint,
    price: uint,
    active: bool
})

;; Public functions
(define-public (create-listing (amount uint) (price uint))
    (let ((listing-id (var-get listing-nonce)))
        (map-set listings listing-id {
            seller: tx-sender,
            amount: amount,
            price: price,
            active: true
        })
        (var-set listing-nonce (+ listing-id u1))
        (ok listing-id)))

(define-public (purchase-listing (listing-id uint))
    (let ((listing (unwrap! (map-get? listings listing-id) ERR-LISTING-NOT-FOUND)))
        (if (get active listing)
            (begin
                (try! (stx-transfer? 
                    (get price listing)
                    tx-sender
                    (get seller listing)))
                (try! (transfer 
                    (get amount listing)
                    (get seller listing)
                    tx-sender))
                (map-set listings listing-id (merge listing {active: false}))
                (ok true))
            ERR-LISTING-NOT-FOUND)))
;; Read-only functions
(define-read-only (get-listing (listing-id uint))
    (map-get? listings listing-id))



;; Add to existing maps
(define-map offset-expiration uint uint) ;; offset-id -> expiration block height

;; New public function
(define-public (set-offset-expiration (offset-id uint) (blocks uint))
    (let ((offset (unwrap! (map-get? verified-offsets offset-id) ERR-INVALID-AMOUNT)))
        (if (is-eq tx-sender (var-get contract-owner))
            (begin
                (map-set offset-expiration offset-id (+ stacks-block-height blocks))
                (ok true))
            ERR-NOT-AUTHORIZED)))


;; New map
(define-map offset-ratings uint {
    rating: uint,
    rater-count: uint
})

;; New public function
(define-public (rate-offset (offset-id uint) (rating uint))
    (let ((current-rating (default-to {rating: u0, rater-count: u0} 
                          (map-get? offset-ratings offset-id))))
        (if (and (>= rating u1) (<= rating u5))
            (begin
                (map-set offset-ratings offset-id {
                    rating: (+ (get rating current-rating) rating),
                    rater-count: (+ (get rater-count current-rating) u1)
                })
                (ok true))
            ERR-INVALID-AMOUNT)))


;; New public function
(define-public (verify-batch (offset-ids (list 10 uint)))
    (let ((owner (var-get contract-owner)))
        (if (is-eq tx-sender owner)
            (begin
                (map verify-single-offset offset-ids)
                (ok true))
            ERR-NOT-AUTHORIZED)))

(define-private (verify-single-offset (offset-id uint))
    (match (map-get? verified-offsets offset-id)
        offset (map-set verified-offsets offset-id (merge offset {verified: true}))
        false))



;; New map
(define-map transfer-history uint (list 10 {
    from: principal,
    to: principal,
    block: uint
}))

;; New public function
(define-public (record-transfer (offset-id uint) (recipient principal))
    (let ((current-history (default-to (list) (map-get? transfer-history offset-id)))
          (new-entry {
              from: tx-sender,
              to: recipient,
              block: stacks-block-height
          }))
        (map-set transfer-history offset-id
            (unwrap-panic (as-max-len? (concat (list new-entry) current-history) u10)))
        (ok true)))



;; New maps
(define-map staked-offsets principal uint)
(define-map staking-rewards principal uint)

;; New public functions
(define-public (stake-offsets (amount uint))
    (let ((current-balance (ft-get-balance carbon-credit tx-sender)))
        (if (>= current-balance amount)
            (begin
                (map-set staked-offsets tx-sender amount)
                (try! (transfer amount tx-sender (as-contract tx-sender)))
                (ok true))
            ERR-INSUFFICIENT-BALANCE)))


(define-public (unstake-offsets)
    (let ((amount (unwrap! (map-get? staked-offsets tx-sender) ERR-INVALID-AMOUNT)))
        (begin
            (try! (mint amount tx-sender))
            (map-set staked-offsets tx-sender u0)
            (ok true)))
    )



;; New maps
(define-map offset-bundles uint {
    offsets: (list 10 uint),
    price: uint,
    available: bool
})

(define-data-var bundle-nonce uint u0)

;; New public function
(define-public (create-bundle (offset-ids (list 10 uint)) (bundle-price uint))
    (let ((bundle-id (var-get bundle-nonce)))
        (map-set offset-bundles bundle-id {
            offsets: offset-ids,
            price: bundle-price,
            available: true
        })
        (var-set bundle-nonce (+ bundle-id u1))
        (ok bundle-id)))


;; New map for quality scores
(define-map offset-quality-scores uint {
    environmental-impact: uint,
    durability: uint,
    community-benefit: uint
})

(define-public (set-quality-score (offset-id uint) 
                                 (environmental uint) 
                                 (durability uint) 
                                 (community uint))
    (let ((offset (unwrap! (map-get? verified-offsets offset-id) ERR-INVALID-AMOUNT)))
        (if (is-eq tx-sender (var-get contract-owner))
            (begin
                (map-set offset-quality-scores offset-id {
                    environmental-impact: environmental,
                    durability: durability,
                    community-benefit: community
                })
                (ok true))
            ERR-NOT-AUTHORIZED)))



;; Define categories
(define-constant CATEGORY-REFORESTATION u1)
(define-constant CATEGORY-RENEWABLE-ENERGY u2)
(define-constant CATEGORY-METHANE-CAPTURE u3)

(define-map offset-categories uint uint)

(define-public (set-offset-category (offset-id uint) (category uint))
    (let ((offset (unwrap! (map-get? verified-offsets offset-id) ERR-INVALID-AMOUNT)))
        (if (and 
            (is-eq tx-sender (var-get contract-owner))
            (<= category u3))
            (begin
                (map-set offset-categories offset-id category)
                (ok true))
            ERR-NOT-AUTHORIZED)))


(define-map referral-rewards principal uint)
(define-constant REFERRAL-BONUS u100)

(define-public (refer-new-user (new-user principal))
    (let ((current-rewards (default-to u0 (map-get? referral-rewards tx-sender))))
        (begin
            (map-set referral-rewards tx-sender (+ current-rewards REFERRAL-BONUS))
            (ok true))))


(define-map staking-lock-time principal uint)
(define-constant MIN-LOCK-PERIOD u1000)
(define-constant LOCK-BONUS-RATE u5)

(define-public (lock-stake (lock-blocks uint))
    (let ((staked-amount (unwrap! (map-get? staked-offsets tx-sender) ERR-INVALID-AMOUNT)))
        (if (>= lock-blocks MIN-LOCK-PERIOD)
            (begin
                (map-set staking-lock-time tx-sender (+ stacks-block-height lock-blocks))
                (ok true))
            ERR-INVALID-AMOUNT)))


(define-constant TIER-BASIC u1)
(define-constant TIER-PREMIUM u2)
(define-constant TIER-GOLD u3)

(define-map offset-tiers uint uint)

(define-public (set-offset-tier (offset-id uint) (tier uint))
    (let ((offset (unwrap! (map-get? verified-offsets offset-id) ERR-INVALID-AMOUNT)))
        (if (and 
            (is-eq tx-sender (var-get contract-owner))
            (<= tier u3))
            (begin
                (map-set offset-tiers offset-id tier)
                (ok true))
            ERR-NOT-AUTHORIZED)))


(define-map price-adjustment-factors uint uint)
(define-constant BASE-PRICE u1000)

(define-public (set-price-adjustment (offset-id uint) (factor uint))
    (let ((offset (unwrap! (map-get? verified-offsets offset-id) ERR-INVALID-AMOUNT)))
        (if (is-eq tx-sender (var-get contract-owner))
            (begin
                (map-set price-adjustment-factors offset-id factor)
                (ok true))
            ERR-NOT-AUTHORIZED)))

(define-read-only (get-adjusted-price (offset-id uint))
    (let ((factor (default-to u100 (map-get? price-adjustment-factors offset-id))))
        (* BASE-PRICE factor)))




(define-map project-milestones uint (list 5 {
    description: (string-utf8 50),
    completed: bool,
    completion-block: uint
}))

(define-public (add-milestone (offset-id uint) (description (string-utf8 50)))
    (let ((current-milestones (default-to (list) (map-get? project-milestones offset-id))))
        (if (is-eq tx-sender (var-get contract-owner))
            (begin
                (map-set project-milestones offset-id
                    (unwrap-panic (as-max-len? 
                        (concat current-milestones (list {
                            description: description,
                            completed: false,
                            completion-block: u0
                        })) u5)))
                (ok true))
            ERR-NOT-AUTHORIZED)))


;; Add new map
(define-map scheduled-retirements uint {
    retirement-height: uint,
    amount: uint,
    beneficiary: principal
})

(define-data-var retirement-nonce uint u0)

;; Schedule retirement
(define-public (schedule-retirement (amount uint) (blocks-until-retirement uint) (beneficiary principal))
    (let 
        ((retirement-id (var-get retirement-nonce))
         (current-balance (ft-get-balance carbon-credit tx-sender)))
        (if (>= current-balance amount)
            (begin
                (try! (transfer amount tx-sender (as-contract tx-sender)))
                (map-set scheduled-retirements retirement-id {
                    retirement-height: (+ stacks-block-height blocks-until-retirement),
                    amount: amount,
                    beneficiary: beneficiary
                })
                (var-set retirement-nonce (+ retirement-id u1))
                (ok retirement-id))
            ERR-INSUFFICIENT-BALANCE)))

(define-public (execute-retirement (retirement-id uint))
    (let ((retirement (unwrap! (map-get? scheduled-retirements retirement-id) ERR-INVALID-AMOUNT)))
        (if (>= stacks-block-height (get retirement-height retirement))
            (begin
                (try! (as-contract (ft-burn? carbon-credit (get amount retirement) (get beneficiary retirement))))
                (ok true))
            ERR-NOT-AUTHORIZED)))



(define-map price-oracle-data uint {
    price: uint,
    timestamp: uint,
    oracle: principal
})

(define-map authorized-oracles principal bool)

(define-data-var last-price uint u0)
(define-data-var price-update-threshold uint u100)

(define-public (register-oracle (oracle principal))
    (if (is-eq tx-sender (var-get contract-owner))
        (begin
            (map-set authorized-oracles oracle true)
            (ok true))
        ERR-NOT-AUTHORIZED))

(define-public (update-price (new-price uint))
    (let ((is-oracle (default-to false (map-get? authorized-oracles tx-sender))))
        (if is-oracle
            (begin
                (map-set price-oracle-data stacks-block-height {
                    price: new-price,
                    timestamp: stacks-block-height,
                    oracle: tx-sender
                })
                (var-set last-price new-price)
                (ok true))
            ERR-NOT-AUTHORIZED)))

(define-read-only (get-current-price)
    (var-get last-price))



(define-map vesting-schedules uint {
    total-amount: uint,
    amount-per-period: uint,
    periods-remaining: uint,
    period-length: uint,
    next-retirement: uint,
    beneficiary: principal
})

(define-data-var schedule-nonce uint u0)

(define-public (create-vesting-schedule 
    (total-amount uint) 
    (periods uint)
    (period-length uint)
    (beneficiary principal))
    (let 
        ((schedule-id (var-get schedule-nonce))
         (amount-per-period (/ total-amount periods)))
        (try! (transfer total-amount tx-sender (as-contract tx-sender)))
        (map-set vesting-schedules schedule-id {
            total-amount: total-amount,
            amount-per-period: amount-per-period,
            periods-remaining: periods,
            period-length: period-length,
            next-retirement: (+ stacks-block-height period-length),
            beneficiary: beneficiary
        })
        (var-set schedule-nonce (+ schedule-id u1))
        (ok schedule-id)))

(define-public (process-vesting-retirement (schedule-id uint))
    (let ((schedule (unwrap! (map-get? vesting-schedules schedule-id) (err u105))))
        (if (and 
            (> (get periods-remaining schedule) u0)
            (>= stacks-block-height (get next-retirement schedule)))
            (begin
                (try! (as-contract (ft-burn? carbon-credit 
                    (get amount-per-period schedule)
                    (get beneficiary schedule))))
                (map-set vesting-schedules schedule-id
                    (merge schedule {
                        periods-remaining: (- (get periods-remaining schedule) u1),
                        next-retirement: (+ (get next-retirement schedule) 
                                          (get period-length schedule))
                    }))
                (ok true))
            ERR-NOT-AUTHORIZED)))



(define-map volume-discount-tiers uint {
    min-amount: uint,
    discount-bps: uint
})

(define-data-var base-price-per-credit uint u1000)
(define-data-var tier-count uint u0)

(define-public (add-discount-tier (min-amount uint) (discount-bps uint))
    (if (is-eq tx-sender (var-get contract-owner))
        (begin
            (map-set volume-discount-tiers (var-get tier-count) {
                min-amount: min-amount,
                discount-bps: discount-bps
            })
            (var-set tier-count (+ (var-get tier-count) u1))
            (ok true))
        ERR-NOT-AUTHORIZED))

(define-read-only (calculate-discounted-price (amount uint))
    (let ((base-total (* amount (var-get base-price-per-credit))))
        (fold check-tier-discount
              (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
              base-total)))
(define-private (check-tier-discount (tier uint) (current-price uint))
    (match (map-get? volume-discount-tiers tier)
        discount-tier
        (if (>= current-price (get min-amount discount-tier))
            (- current-price 
               (/ (* current-price (get discount-bps discount-tier)) u10000))
            current-price)
        current-price))