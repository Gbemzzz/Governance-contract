;; Governance contract for managing the blockchain relief fund

;; =========================================
;; Data variables
;; =========================================

(define-data-var admin principal tx-sender)
(define-data-var min-donation uint u10)
(define-data-var withdrawal-limit uint u1000)
(define-data-var required-signatures uint u3)
(define-data-var tx-nonce uint u0)
(define-data-var paused bool false)

;; =========================================
;; Maps
;; =========================================

(define-map signers principal bool)

(define-map pending-transactions
 { tx-id: uint }
 { action: (string-ascii 50), params: (list 10 int), approvals: (list 10 principal) })

(define-map donations
 { donor: principal }
 { total-donated: uint })

(define-map withdrawals
 { recipient: principal }
 { total-withdrawn: uint })

(define-map beneficiaries
 { account: principal }
 { registered: bool, total-allocated: uint, total-claimed: uint })

(define-map allocations
 { account: principal }
 { available: uint })

(define-map donation-history
 { id: uint }
 { donor: principal, amount: uint, block: uint })

(define-map withdrawal-history
 { id: uint }
 { recipient: principal, amount: uint, block: uint })

;; =========================================
;; Counters
;; =========================================

(define-data-var donation-counter uint u0)
(define-data-var withdrawal-counter uint u0)

;; =========================================
;; Core functions
;; =========================================

(define-public (set-admin (new-admin principal))
 (let ((current-admin (var-get admin)))
   (if (and
         (is-eq tx-sender current-admin)
         (not (is-eq new-admin current-admin))
         (not (is-eq new-admin 'SP000000000000000000002Q6VF78)))
     (begin
       (var-set admin new-admin)
       (ok new-admin))
     (err u401))))

(define-public (set-min-donation (amount uint))
 (if (is-eq tx-sender (var-get admin))
   (if (> amount u0)
     (begin (var-set min-donation amount) (ok amount))
     (err u402))
   (err u401)))

(define-public (set-withdrawal-limit (amount uint))
 (if (is-eq tx-sender (var-get admin))
   (if (> amount u0)
     (begin (var-set withdrawal-limit amount) (ok amount))
     (err u403))
   (err u401)))

(define-read-only (get-admin) (ok (var-get admin)))
(define-read-only (get-min-donation) (ok (var-get min-donation)))
(define-read-only (get-withdrawal-limit) (ok (var-get withdrawal-limit)))
(define-read-only (is-paused) (ok (var-get paused)))

;; =========================================
;; Restrictions for disaster relief
;; =========================================

(define-public (validate-donation (amount uint))
 (if (>= amount (var-get min-donation)) (ok true) (err u404)))

(define-public (validate-withdrawal (amount uint))
 (if (<= amount (var-get withdrawal-limit)) (ok true) (err u405)))

;; =========================================
;; Pause control
;; =========================================

(define-public (pause)
 (begin
   (asserts! (is-eq tx-sender (var-get admin)) (err u401))
   (var-set paused true)
   (ok true)))

(define-public (unpause)
 (begin
   (asserts! (is-eq tx-sender (var-get admin)) (err u401))
   (var-set paused false)
   (ok true)))

;; =========================================
;; Multi-signature governance
;; =========================================

(define-public (add-signer (new-signer principal))
 (begin
   (asserts! (is-eq tx-sender (var-get admin)) (err u401))
   (asserts! (is-none (map-get? signers new-signer)) (err u403))
   (map-set signers new-signer true)
   (ok true)))

(define-public (remove-signer (signer principal))
 (begin
   (asserts! (is-eq tx-sender (var-get admin)) (err u401))
   (asserts! (is-some (map-get? signers signer)) (err u404))
   (map-delete signers signer)
   (ok true)))

(define-public (change-admin (new-admin principal))
 (begin
   (asserts! (is-eq tx-sender (var-get admin)) (err u401))
   (asserts! (not (is-eq new-admin (var-get admin))) (err u403))
   (asserts! (not (is-eq new-admin 'SP000000000000000000002Q6VF78)) (err u404))
   (var-set admin new-admin)
   (ok true)))

(define-public (propose-transaction (action (string-ascii 50)) (params (list 10 int)))
 (let ((tx-id (var-get tx-nonce)) (action-length (len action)))
   (asserts! (is-some (map-get? signers tx-sender)) (err u401))
   (asserts! (and (> action-length u0) (<= action-length u50)) (err u402))
   (asserts! (<= (len params) u10) (err u403))
   (map-set pending-transactions
     { tx-id: tx-id }
     { action: action, params: params, approvals: (list tx-sender) })
   (var-set tx-nonce (+ tx-id u1))
   (ok tx-id)))

(define-read-only (get-pending-transaction (tx-id uint))
 (map-get? pending-transactions { tx-id: tx-id }))

(define-read-only (get-tx-nonce) (ok (var-get tx-nonce)))

(define-private (execute-transaction (tx-id uint))
 (let ((tx (unwrap! (map-get? pending-transactions { tx-id: tx-id }) (err u404))))
   (map-delete pending-transactions { tx-id: tx-id })
   (ok true)))

(define-read-only (is-signer (address principal))
 (is-some (map-get? signers address)))

(define-read-only (get-required-signatures) (ok (var-get required-signatures)))

(define-public (set-required-signatures (new-required uint))
 (begin
   (asserts! (is-eq tx-sender (var-get admin)) (err u401))
   (asserts! (> new-required u0) (err u403))
   (var-set required-signatures new-required)
   (ok true)))

;; =========================================
;; Donation and withdrawal features
;; =========================================

(define-public (donate (amount uint))
 (begin
   (asserts! (not (var-get paused)) (err u406))
   (asserts! (>= amount (var-get min-donation)) (err u404))
   (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
   (match (map-get? donations { donor: tx-sender })
     donor-data
       (map-set donations { donor: tx-sender }
         { total-donated: (+ amount (get total-donated donor-data)) })
     (map-set donations { donor: tx-sender } { total-donated: amount }))
   (let ((id (var-get donation-counter)))
     (map-set donation-history { id: id } { donor: tx-sender, amount: amount, block: block-height })
     (var-set donation-counter (+ id u1)))
   (ok true)))

(define-public (withdraw (recipient principal) (amount uint))
 (begin
   (asserts! (not (var-get paused)) (err u406))
   (asserts! (<= amount (var-get withdrawal-limit)) (err u405))
   (let ((prev (default-to u0 (get total-withdrawn (map-get? withdrawals { recipient: recipient })))))
     (map-set withdrawals { recipient: recipient } { total-withdrawn: (+ prev amount) })
     (try! (as-contract (stx-transfer? amount tx-sender recipient)))
     (let ((wid (var-get withdrawal-counter)))
       (map-set withdrawal-history { id: wid } { recipient: recipient, amount: amount, block: block-height })
       (var-set withdrawal-counter (+ wid u1)))
     (ok true))))

(define-read-only (get-donation (donor principal))
 (default-to u0 (get total-donated (map-get? donations { donor: donor })) ))

(define-read-only (get-withdrawn (recipient principal))
 (default-to u0 (get total-withdrawn (map-get? withdrawals { recipient: recipient })) ))

(define-read-only (get-contract-balance)
 (stx-get-balance (as-contract tx-sender)))

;; =========================================
;; Beneficiaries and allocations
;; =========================================

(define-public (register-beneficiary (account principal))
 (begin
   (asserts! (is-eq tx-sender (var-get admin)) (err u401))
   (match (map-get? beneficiaries { account: account })
     b (begin (asserts! (not (get registered b)) (err u403)) (ok false))
     (begin
       (map-set beneficiaries { account: account } { registered: true, total-allocated: u0, total-claimed: u0 })
       (map-set allocations { account: account } { available: u0 })
       (ok true)))))

(define-public (allocate (account principal) (amount uint))
 (begin
   (asserts! (is-eq tx-sender (var-get admin)) (err u401))
   (asserts! (not (var-get paused)) (err u406))
   (let ((b (unwrap! (map-get? beneficiaries { account: account }) (err u404))))
     (asserts! (get registered b) (err u404))
     (let ((alloc (default-to u0 (get available (map-get? allocations { account: account })) )))
       (map-set allocations { account: account } { available: (+ alloc amount) })
       (map-set beneficiaries { account: account }
         { registered: true, total-allocated: (+ (get total-allocated b) amount), total-claimed: (get total-claimed b) })
       (ok true)))))

(define-public (beneficiary-withdraw (amount uint))
 (begin
   (asserts! (not (var-get paused)) (err u406))
   (let ((b (unwrap! (map-get? beneficiaries { account: tx-sender }) (err u404))))
     (asserts! (get registered b) (err u404))
     (let ((alloc (default-to u0 (get available (map-get? allocations { account: tx-sender })) )))
       (asserts! (<= amount alloc) (err u405))
       (asserts! (<= amount (var-get withdrawal-limit)) (err u405))
       (map-set allocations { account: tx-sender } { available: (- alloc amount) })
       (map-set beneficiaries { account: tx-sender }
         { registered: true, total-allocated: (get total-allocated b), total-claimed: (+ (get total-claimed b) amount) })
       (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
       (let ((wid (var-get withdrawal-counter)))
         (map-set withdrawal-history { id: wid } { recipient: tx-sender, amount: amount, block: block-height })
         (var-set withdrawal-counter (+ wid u1)))
       (ok true)))))

(define-read-only (get-beneficiary (account principal))
 (map-get? beneficiaries { account: account }))

(define-read-only (get-allocation (account principal))
 (default-to u0 (get available (map-get? allocations { account: account }))))