;; Governance contract for managing the blockchain relief fund

;; =========================================
;; Constants
;; =========================================

(define-constant CONTRACT-NAME "ReliefFund Gov")
(define-constant CONTRACT-VERSION "1.0.0")
(define-constant CONTRACT-PURPOSE "Disaster relief governance and fund management")

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-AMOUNT (err u402))
(define-constant ERR-ALREADY-EXISTS (err u403))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT-BALANCE (err u405))
(define-constant ERR-CONTRACT-PAUSED (err u406))
(define-constant ERR-ARITHMETIC-OVERFLOW (err u407))
(define-constant ERR-INVALID-PARAMETER (err u500))

;; =========================================
;; Data variables
;; =========================================

(define-data-var admin principal tx-sender)
(define-data-var min-donation uint u10)
(define-data-var withdrawal-limit uint u1000)
(define-data-var required-signatures uint u3)
(define-data-var tx-nonce uint u0)
(define-data-var paused bool false)
(define-data-var donation-counter uint u0)
(define-data-var withdrawal-counter uint u0)

;; =========================================
;; Maps
;; =========================================

(define-map signers principal bool)

(define-map pending-transactions
  { tx-id: uint }
  { action: (string-ascii 50), params: (list 10 uint), approval-count: uint })

(define-map approvals
  { tx-id: uint, signer: principal }
  bool)

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
;; Helper functions
;; =========================================

(define-private (is-admin)
  (is-eq tx-sender (var-get admin)))

(define-private (is-authorized-signer)
  (is-some (map-get? signers tx-sender)))

(define-private (safe-add (a uint) (b uint))
  (let ((result (+ a b)))
    (if (and (>= result a) (>= result b))
      (ok result)
      ERR-ARITHMETIC-OVERFLOW)))

(define-private (safe-sub (a uint) (b uint))
  (if (>= a b) 
    (ok (- a b)) 
    ERR-INSUFFICIENT-BALANCE))

;; =========================================
;; Core admin functions
;; =========================================

(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (asserts! (not (is-eq new-admin (var-get admin))) ERR-INVALID-PARAMETER)
    (asserts! (not (is-eq new-admin 'SP000000000000000000002Q6VF78)) ERR-INVALID-PARAMETER)
    (var-set admin new-admin)
    (ok new-admin)))

(define-public (set-min-donation (amount uint))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (var-set min-donation amount)
    (ok amount)))

(define-public (set-withdrawal-limit (amount uint))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (var-set withdrawal-limit amount)
    (ok amount)))

(define-public (set-required-signatures (new-required uint))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (asserts! (> new-required u0) ERR-INVALID-PARAMETER)
    (var-set required-signatures new-required)
    (ok true)))

;; =========================================
;; Read-only functions
;; =========================================

(define-read-only (get-admin) 
  (var-get admin))

(define-read-only (get-min-donation) 
  (var-get min-donation))

(define-read-only (get-withdrawal-limit) 
  (var-get withdrawal-limit))

(define-read-only (is-paused) 
  (var-get paused))

(define-read-only (get-required-signatures) 
  (var-get required-signatures))

(define-read-only (get-tx-nonce) 
  (var-get tx-nonce))

(define-read-only (is-signer (address principal))
  (is-some (map-get? signers address)))

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender)))

;; =========================================
;; Validation functions
;; =========================================

(define-public (validate-donation (amount uint))
  (if (>= amount (var-get min-donation)) 
    (ok true) 
    ERR-INVALID-AMOUNT))

(define-public (validate-withdrawal (amount uint))
  (if (<= amount (var-get withdrawal-limit)) 
    (ok true) 
    ERR-INVALID-AMOUNT))

;; =========================================
;; Pause control
;; =========================================

(define-public (pause)
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (var-set paused true)
    (ok true)))

(define-public (unpause)
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (var-set paused false)
    (ok true)))

;; =========================================
;; Multi-signature governance
;; =========================================

(define-public (add-signer (new-signer principal))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (asserts! (is-none (map-get? signers new-signer)) ERR-ALREADY-EXISTS)
    (map-set signers new-signer true)
    (ok true)))

(define-public (remove-signer (signer principal))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (asserts! (is-some (map-get? signers signer)) ERR-NOT-FOUND)
    (map-delete signers signer)
    (ok true)))

(define-public (propose-transaction (action (string-ascii 50)) (params (list 10 uint)))
  (let ((tx-id (var-get tx-nonce)))
    (asserts! (is-authorized-signer) ERR-UNAUTHORIZED)
    (asserts! (> (len action) u0) ERR-INVALID-PARAMETER)
    (asserts! (<= (len action) u50) ERR-INVALID-PARAMETER)
    (asserts! (<= (len params) u10) ERR-INVALID-PARAMETER)
    (map-set pending-transactions
             { tx-id: tx-id }
             { action: action, params: params, approval-count: u1 })
    (map-set approvals { tx-id: tx-id, signer: tx-sender } true)
    (var-set tx-nonce (+ tx-id u1))
    (ok tx-id)))

(define-public (approve-transaction (tx-id uint))
  (let ((tx-data (unwrap! (map-get? pending-transactions { tx-id: tx-id }) ERR-NOT-FOUND)))
    (asserts! (is-authorized-signer) ERR-UNAUTHORIZED)
    (asserts! (is-none (map-get? approvals { tx-id: tx-id, signer: tx-sender })) ERR-ALREADY-EXISTS)
    (map-set approvals { tx-id: tx-id, signer: tx-sender } true)
    (let ((curr-count (get approval-count tx-data))
          (new-count (+ curr-count u1)))
      (map-set pending-transactions 
               { tx-id: tx-id } 
               { action: (get action tx-data), 
                 params: (get params tx-data), 
                 approval-count: new-count })
      (if (>= new-count (var-get required-signatures))
        (execute-transaction tx-id)
        (ok true)))))

(define-read-only (get-pending-transaction (tx-id uint))
  (map-get? pending-transactions { tx-id: tx-id }))

;; =========================================
;; Donation and withdrawal features
;; =========================================

(define-public (donate (amount uint))
  (begin
    (asserts! (not (var-get paused)) ERR-CONTRACT-PAUSED)
    (asserts! (>= amount (var-get min-donation)) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let ((current-total (default-to u0 (get total-donated (map-get? donations { donor: tx-sender })))))
      (let ((new-total (unwrap! (safe-add current-total amount) ERR-ARITHMETIC-OVERFLOW)))
        (map-set donations { donor: tx-sender } { total-donated: new-total })))
    (let ((id (var-get donation-counter)))
      (map-set donation-history { id: id } 
               { donor: tx-sender, amount: amount, block: block-height })
      (var-set donation-counter (+ id u1)))
    (ok true)))

(define-public (withdraw (recipient principal) (amount uint))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (asserts! (not (var-get paused)) ERR-CONTRACT-PAUSED)
    (asserts! (<= amount (var-get withdrawal-limit)) ERR-INVALID-AMOUNT)
    (asserts! (>= (get-contract-balance) amount) ERR-INSUFFICIENT-BALANCE)
    (let ((prev-total (default-to u0 (get total-withdrawn (map-get? withdrawals { recipient: recipient })))))
      (let ((new-total (unwrap! (safe-add prev-total amount) ERR-ARITHMETIC-OVERFLOW)))
        (map-set withdrawals { recipient: recipient } { total-withdrawn: new-total })))
    (try! (stx-transfer? amount (as-contract tx-sender) recipient))
    (let ((wid (var-get withdrawal-counter)))
      (map-set withdrawal-history { id: wid } 
               { recipient: recipient, amount: amount, block: block-height })
      (var-set withdrawal-counter (+ wid u1)))
    (ok true)))

(define-read-only (get-donation (donor principal))
  (default-to u0 (get total-donated (map-get? donations { donor: donor }))))

(define-read-only (get-withdrawn (recipient principal))
  (default-to u0 (get total-withdrawn (map-get? withdrawals { recipient: recipient }))))

;; =========================================
;; Beneficiaries and allocations
;; =========================================

(define-public (register-beneficiary (account principal))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (asserts! (is-none (map-get? beneficiaries { account: account })) ERR-ALREADY-EXISTS)
    (map-set beneficiaries { account: account } 
             { registered: true, total-allocated: u0, total-claimed: u0 })
    (map-set allocations { account: account } { available: u0 })
    (ok true)))

(define-public (allocate (account principal) (amount uint))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED)
    (asserts! (not (var-get paused)) ERR-CONTRACT-PAUSED)
    (let ((b (unwrap! (map-get? beneficiaries { account: account }) ERR-NOT-FOUND)))
      (asserts! (get registered b) ERR-NOT-FOUND)
      (let ((current-alloc (default-to u0 (get available (map-get? allocations { account: account })))))
        (let ((new-alloc (unwrap! (safe-add current-alloc amount) ERR-ARITHMETIC-OVERFLOW))
              (new-total-allocated (unwrap! (safe-add (get total-allocated b) amount) ERR-ARITHMETIC-OVERFLOW)))
          (map-set allocations { account: account } { available: new-alloc })
          (map-set beneficiaries { account: account } 
                   { registered: true, 
                     total-allocated: new-total-allocated, 
                     total-claimed: (get total-claimed b) })
          (ok true))))))

(define-public (beneficiary-withdraw (amount uint))
  (begin
    (asserts! (not (var-get paused)) ERR-CONTRACT-PAUSED)
    (let ((b (unwrap! (map-get? beneficiaries { account: tx-sender }) ERR-NOT-FOUND)))
      (asserts! (get registered b) ERR-NOT-FOUND)
      (let ((alloc (default-to u0 (get available (map-get? allocations { account: tx-sender })))))
        (asserts! (<= amount alloc) ERR-INSUFFICIENT-BALANCE)
        (asserts! (<= amount (var-get withdrawal-limit)) ERR-INVALID-AMOUNT)
        (let ((new-alloc (unwrap! (safe-sub alloc amount) ERR-INSUFFICIENT-BALANCE))
              (new-claimed (unwrap! (safe-add (get total-claimed b) amount) ERR-ARITHMETIC-OVERFLOW)))
          (map-set allocations { account: tx-sender } { available: new-alloc })
          (map-set beneficiaries { account: tx-sender } 
                   { registered: true, 
                     total-allocated: (get total-allocated b), 
                     total-claimed: new-claimed })
          (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
          (let ((wid (var-get withdrawal-counter)))
            (map-set withdrawal-history { id: wid } 
                     { recipient: tx-sender, amount: amount, block: block-height })
            (var-set withdrawal-counter (+ wid u1)))
          (ok true))))))

(define-read-only (get-beneficiary (account principal))
  (map-get? beneficiaries { account: account }))

(define-read-only (get-allocation (account principal))
  (default-to u0 (get available (map-get? allocations { account: account }))))

;; =========================================
;; Proposal execution functions
;; =========================================

(define-private (exec-set-min-donation (amount uint))
  (begin 
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (var-set min-donation amount) 
    (ok true)))

(define-private (exec-set-withdrawal-limit (amount uint))
  (begin 
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (var-set withdrawal-limit amount) 
    (ok true)))

(define-private (exec-pause) 
  (begin 
    (var-set paused true) 
    (ok true)))

(define-private (exec-unpause) 
  (begin 
    (var-set paused false) 
    (ok true)))

(define-private (exec-emergency-sweep (to principal))
  (begin
    (let ((bal (stx-get-balance (as-contract tx-sender))))
      (asserts! (> bal u0) ERR-INSUFFICIENT-BALANCE)
      (try! (stx-transfer? bal (as-contract tx-sender) to))
      (ok true))))

(define-private (execute-transaction (tx-id uint))
  (let ((tx-data (unwrap! (map-get? pending-transactions { tx-id: tx-id }) ERR-NOT-FOUND)))
    (map-delete pending-transactions { tx-id: tx-id })
    (let ((action (get action tx-data)) 
          (params (get params tx-data)))
      (if (is-eq action "set-min-donation")
        (match (element-at params u0)
          p0 (exec-set-min-donation p0)
          ERR-INVALID-PARAMETER)
        (if (is-eq action "set-withdrawal-limit")
          (match (element-at params u0)
            p0 (exec-set-withdrawal-limit p0)
            ERR-INVALID-PARAMETER)
          (if (is-eq action "pause")
            (exec-pause)
            (if (is-eq action "unpause")
              (exec-unpause)
              (if (is-eq action "emergency-sweep")
                (exec-emergency-sweep (var-get admin))
                ERR-INVALID-PARAMETER))))))))

;; =========================================
;; View and utility functions
;; =========================================

(define-read-only (get-metadata)
  { name: CONTRACT-NAME, version: CONTRACT-VERSION, purpose: CONTRACT-PURPOSE })

(define-read-only (get-params)
  { admin: (var-get admin),
    min-donation: (var-get min-donation),
    withdrawal-limit: (var-get withdrawal-limit),
    required-signatures: (var-get required-signatures),
    paused: (var-get paused) })

(define-read-only (get-stats)
  { donations: (var-get donation-counter),
    withdrawals: (var-get withdrawal-counter),
    balance: (stx-get-balance (as-contract tx-sender)) })

(define-read-only (can-beneficiary-withdraw (account principal) (amount uint))
  (let ((alloc (default-to u0 (get available (map-get? allocations { account: account })))))
    (and (<= amount alloc) 
         (<= amount (var-get withdrawal-limit)) 
         (not (var-get paused)))))
         