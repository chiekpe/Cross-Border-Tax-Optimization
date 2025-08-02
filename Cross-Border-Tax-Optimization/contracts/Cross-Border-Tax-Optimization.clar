;; Cross-Border Tax Optimization Contract
;; Automated tax-efficient international payments with compliance tracking

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INVALID_COUNTRY (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_PAYMENT_NOT_FOUND (err u104))
(define-constant ERR_ALREADY_PROCESSED (err u105))
(define-constant ERR_INVALID_TAX_RATE (err u106))
(define-constant ERR_COMPLIANCE_FAILURE (err u107))

;; Data Variables
(define-data-var contract-active bool true)
(define-data-var total-payments-processed uint u0)
(define-data-var total-tax-saved uint u0)

;; Data Maps
(define-map tax-rates 
    { from-country: (string-ascii 3), to-country: (string-ascii 3) }
    { withholding-rate: uint, treaty-rate: uint, exemption-threshold: uint })

(define-map user-balances principal uint)

(define-map payment-records 
    uint 
    { 
        sender: principal,
        recipient: principal,
        amount: uint,
        from-country: (string-ascii 3),
        to-country: (string-ascii 3),
        tax-withheld: uint,
        tax-saved: uint,
        timestamp: uint,
        status: (string-ascii 20),
        route-used: (string-ascii 50)
    })

(define-map user-compliance-status
    principal
    {
        kyc-verified: bool,
        tax-resident-country: (string-ascii 3),
        compliance-score: uint,
        last-updated: uint
    })

(define-map country-routes
    { from: (string-ascii 3), to: (string-ascii 3) }
    { 
        primary-route: (string-ascii 50),
        backup-route: (string-ascii 50),
        efficiency-score: uint
    })

;; Read-only functions
(define-read-only (get-contract-info)
    {
        active: (var-get contract-active),
        total-payments: (var-get total-payments-processed),
        total-tax-saved: (var-get total-tax-saved)
    })

(define-read-only (get-tax-rate (from-country (string-ascii 3)) (to-country (string-ascii 3)))
    (map-get? tax-rates { from-country: from-country, to-country: to-country }))

(define-read-only (get-user-balance (user principal))
    (default-to u0 (map-get? user-balances user)))

(define-read-only (get-payment-record (payment-id uint))
    (map-get? payment-records payment-id))

(define-read-only (get-compliance-status (user principal))
    (map-get? user-compliance-status user))

(define-read-only (calculate-optimal-tax (amount uint) (from-country (string-ascii 3)) (to-country (string-ascii 3)))
    (let 
        (
            (tax-info (unwrap! (get-tax-rate from-country to-country) (err u0)))
            (withholding-rate (get withholding-rate tax-info))
            (treaty-rate (get treaty-rate tax-info))
            (exemption-threshold (get exemption-threshold tax-info))
            (standard-tax (/ (* amount withholding-rate) u10000))
            (treaty-tax (if (>= amount exemption-threshold) 
                           (/ (* amount treaty-rate) u10000) 
                           u0))
            (optimal-tax (if (< treaty-tax standard-tax) treaty-tax standard-tax))
            (tax-saved (- standard-tax optimal-tax))
        )
        (ok { optimal-tax: optimal-tax, tax-saved: tax-saved, route: "treaty" })))

(define-read-only (get-optimal-route (from-country (string-ascii 3)) (to-country (string-ascii 3)))
    (default-to 
        { primary-route: "direct", backup-route: "correspondent", efficiency-score: u50 }
        (map-get? country-routes { from: from-country, to: to-country })))

;; Private functions
(define-private (is-authorized (user principal))
    (or (is-eq user CONTRACT_OWNER) (var-get contract-active)))

(define-private (validate-compliance (user principal))
    (let 
        (
            (compliance (unwrap! (get-compliance-status user) false))
            (kyc-status (get kyc-verified compliance))
            (compliance-score (get compliance-score compliance))
        )
        (and kyc-status (>= compliance-score u70))))

(define-private (update-payment-stats (tax-saved uint))
    (begin
        (var-set total-payments-processed (+ (var-get total-payments-processed) u1))
        (var-set total-tax-saved (+ (var-get total-tax-saved) tax-saved))
        true))

;; Public functions
(define-public (set-tax-rate (from-country (string-ascii 3)) (to-country (string-ascii 3)) 
                            (withholding-rate uint) (treaty-rate uint) (exemption-threshold uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= withholding-rate u10000) ERR_INVALID_TAX_RATE)
        (asserts! (<= treaty-rate u10000) ERR_INVALID_TAX_RATE)
        (ok (map-set tax-rates 
            { from-country: from-country, to-country: to-country }
            { 
                withholding-rate: withholding-rate, 
                treaty-rate: treaty-rate, 
                exemption-threshold: exemption-threshold 
            }))))

(define-public (deposit (amount uint))
    (let
        (
            (current-balance (get-user-balance tx-sender))
            (new-balance (+ current-balance amount))
        )
        (begin
            (asserts! (> amount u0) ERR_INVALID_AMOUNT)
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            (ok (map-set user-balances tx-sender new-balance)))))

(define-public (update-compliance-status (user principal) (kyc-verified bool) 
                                       (tax-resident-country (string-ascii 3)) (compliance-score uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= compliance-score u100) ERR_INVALID_TAX_RATE)
        (ok (map-set user-compliance-status user
            {
                kyc-verified: kyc-verified,
                tax-resident-country: tax-resident-country,
                compliance-score: compliance-score,
                last-updated: block-height
            }))))

(define-public (set-country-route (from-country (string-ascii 3)) (to-country (string-ascii 3))
                                 (primary-route (string-ascii 50)) (backup-route (string-ascii 50)) 
                                 (efficiency-score uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= efficiency-score u100) ERR_INVALID_TAX_RATE)
        (ok (map-set country-routes 
            { from: from-country, to: to-country }
            { 
                primary-route: primary-route, 
                backup-route: backup-route, 
                efficiency-score: efficiency-score 
            }))))

(define-public (process-cross-border-payment (recipient principal) (amount uint) 
                                            (from-country (string-ascii 3)) (to-country (string-ascii 3)))
    (let
        (
            (payment-id (+ (var-get total-payments-processed) u1))
            (sender-balance (get-user-balance tx-sender))
            (tax-calculation (unwrap! (calculate-optimal-tax amount from-country to-country) ERR_INVALID_COUNTRY))
            (optimal-tax (get optimal-tax tax-calculation))
            (tax-saved (get tax-saved tax-calculation))
            (net-amount (- amount optimal-tax))
            (route-info (get-optimal-route from-country to-country))
            (route-used (get primary-route route-info))
        )
        (begin
            (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
            (asserts! (> amount u0) ERR_INVALID_AMOUNT)
            (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_BALANCE)
            (asserts! (validate-compliance tx-sender) ERR_COMPLIANCE_FAILURE)
            (asserts! (validate-compliance recipient) ERR_COMPLIANCE_FAILURE)
            
            ;; Update balances
            (map-set user-balances tx-sender (- sender-balance amount))
            (map-set user-balances recipient (+ (get-user-balance recipient) net-amount))
            
            ;; Record payment
            (map-set payment-records payment-id
                {
                    sender: tx-sender,
                    recipient: recipient,
                    amount: amount,
                    from-country: from-country,
                    to-country: to-country,
                    tax-withheld: optimal-tax,
                    tax-saved: tax-saved,
                    timestamp: block-height,
                    status: "completed",
                    route-used: route-used
                })
            
            ;; Update statistics
            (update-payment-stats tax-saved)
            
            (ok { 
                payment-id: payment-id, 
                net-amount: net-amount, 
                tax-withheld: optimal-tax,
                tax-saved: tax-saved,
                route: route-used
            }))))

(define-public (withdraw (amount uint))
    (let
        (
            (user-balance (get-user-balance tx-sender))
        )
        (begin
            (asserts! (> amount u0) ERR_INVALID_AMOUNT)
            (asserts! (>= user-balance amount) ERR_INSUFFICIENT_BALANCE)
            (map-set user-balances tx-sender (- user-balance amount))
            (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
            (ok amount))))

(define-public (emergency-pause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (ok (var-set contract-active false))))

(define-public (emergency-resume)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (ok (var-set contract-active true))))

;; Initialize some sample tax rates and routes
(map-set tax-rates { from-country: "USA", to-country: "GBR" } 
         { withholding-rate: u3000, treaty-rate: u500, exemption-threshold: u1000000 })
(map-set tax-rates { from-country: "GBR", to-country: "DEU" } 
         { withholding-rate: u2500, treaty-rate: u0, exemption-threshold: u500000 })
(map-set tax-rates { from-country: "USA", to-country: "SGP" } 
         { withholding-rate: u3000, treaty-rate: u1500, exemption-threshold: u2000000 })

(map-set country-routes { from: "USA", to: "GBR" }
         { primary-route: "USD-GBP-Direct", backup-route: "USD-EUR-GBP", efficiency-score: u90 })
(map-set country-routes { from: "GBR", to: "DEU" }
         { primary-route: "GBP-EUR-Direct", backup-route: "GBP-USD-EUR", efficiency-score: u95 })
(map-set country-routes { from: "USA", to: "SGP" }
         { primary-route: "USD-SGD-Swift", backup-route: "USD-HKD-SGD", efficiency-score: u85 })