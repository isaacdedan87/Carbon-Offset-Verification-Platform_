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
