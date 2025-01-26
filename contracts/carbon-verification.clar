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
