;; Governance contract for managing the blockchain relief fund

;; =========================================
;; Metadata
;; =========================================

(define-constant CONTRACT-NAME "ReliefFund Gov")
(define-constant CONTRACT-VERSION "1.0.0")
(define-constant CONTRACT-PURPOSE "Disaster relief governance and fund management")

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
  { action: (string-ascii 50), params: (list 10 int), approval-count: uint })

(define-map approvals
  { tx-id: uint, signer: principal }
  { approved: bool })

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
;; Helpers
;; =========================================

(define-private (uadd (a uint) (b uint)) (+ a b))
(define-private (usub (a uint) (b uint)) (begin (asserts! (>= a b) (err u407)) (- a b)))
(define-private (umin (a uint) (b uint)) (if (<= a b) a b))
(define-private (umax (a uint) (b uint)) (if (>= a b) a b))

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
             { action: action, params: params, approval-count: u1 })
    (map-set approvals { tx-id: tx-id, signer: tx-sender } { approved: true })
    (var-set tx-nonce (+ tx-id u1))
    (ok tx-id)))

(define-public (approve-transaction (tx-id uint))
  (let ((tx (unwrap! (map-get? pending-transactions { tx-id: tx-id }) (err u404))))
    (asserts! (is-some (map-get? signers tx-sender)) (err u401))
    (let ((already (map-get? approvals { tx-id: tx-id, signer: tx-sender })))
      (match already
        approved-entry (err u403)
        (begin
          ;; mark this signer as approved
          (map-set approvals { tx-id: tx-id, signer: tx-sender } { approved: true })
          ;; increment approval count
          (let ((curr-count (default-to u0 (get approval-count (map-get? pending-transactions { tx-id: tx-id })))))
            (map-set pending-transactions { tx-id: tx-id } { action: (get action tx), params: (get params tx), approval-count: (+ curr-count u1) })
            (let ((new-count (+ curr-count u1)))
              (if (>= new-count (var-get required-signatures))
                  (begin
                    (try! (execute-transaction tx-id))
                    (ok true))
                  (ok true)))))))))

(define-read-only (get-pending-transaction (tx-id uint))
  (map-get? pending-transactions { tx-id: tx-id }))

(define-read-only (get-tx-nonce) (ok (var-get tx-nonce)))

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
        (map-set donations { donor: tx-sender } { total-donated: (uadd amount (get total-donated donor-data)) })
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
      (map-set withdrawals { recipient: recipient } { total-withdrawn: (uadd prev amount) })
      (try! (stx-transfer? amount (as-contract tx-sender) recipient))
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
        (map-set allocations { account: account } { available: (uadd alloc amount) })
        (map-set beneficiaries { account: account } { registered: true, total-allocated: (uadd (get total-allocated b) amount), total-claimed: (get total-claimed b) })
        (ok true)))))

(define-public (beneficiary-withdraw (amount uint))
  (begin
    (asserts! (not (var-get paused)) (err u406))
    (let ((b (unwrap! (map-get? beneficiaries { account: tx-sender }) (err u404))))
      (asserts! (get registered b) (err u404))
      (let ((alloc (default-to u0 (get available (map-get? allocations { account: tx-sender })) )))
        (asserts! (<= amount alloc) (err u405))
        (asserts! (<= amount (var-get withdrawal-limit)) (err u405))
        (map-set allocations { account: tx-sender } { available: (usub alloc amount) })
        (map-set beneficiaries { account: tx-sender } { registered: true, total-allocated: (get total-allocated b), total-claimed: (uadd (get total-claimed b) amount) })
        (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
        (let ((wid (var-get withdrawal-counter)))
          (map-set withdrawal-history { id: wid } { recipient: tx-sender, amount: amount, block: block-height })
          (var-set withdrawal-counter (+ wid u1)))
        (ok true)))))

(define-read-only (get-beneficiary (account principal))
  (map-get? beneficiaries { account: account }))

(define-read-only (get-allocation (account principal))
  (default-to u0 (get available (map-get? allocations { account: account })) ))

;; =========================================
;; Proposal execution routes
;; =========================================

(define-private (exec-set-min-donation (amount uint))
  (begin (var-set min-donation amount) (ok true)))

(define-private (exec-set-withdrawal-limit (amount uint))
  (begin (var-set withdrawal-limit amount) (ok true)))

(define-private (exec-pause) (begin (var-set paused true) (ok true)))
(define-private (exec-unpause) (begin (var-set paused false) (ok true)))

(define-private (exec-emergency-sweep (to principal))
  (begin
    (let ((bal (stx-get-balance (as-contract tx-sender))))
      (asserts! (> bal u0) (err u403))
      (try! (stx-transfer? bal (as-contract tx-sender) to))
      (ok true))))

(define-private (execute-transaction (tx-id uint))
  (let ((tx (unwrap! (map-get? pending-transactions { tx-id: tx-id }) (err u404))))
    (map-delete pending-transactions { tx-id: tx-id })
    (let ((action (get action tx)) (params (get params tx)))
      (if (is-eq action "set-min-donation")
        (let ((p0 (element-at params u0)))
          (try! (exec-set-min-donation (to-uint p0)))
          (ok true))
        (if (is-eq action "set-withdrawal-limit")
          (let ((p0 (element-at params u0)))
            (try! (exec-set-withdrawal-limit (to-uint p0)))
            (ok true))
          (if (is-eq action "pause")
            (begin (try! (exec-pause)) (ok true))
            (if (is-eq action "unpause")
              (begin (try! (exec-unpause)) (ok true))
              (if (is-eq action "emergency-sweep")
                (let ((p0 (element-at params u0)))
                  (try! (exec-emergency-sweep (to-principal p0)))
                  (ok true))
                (err u402))))))))

;; =========================================
;; Views and utilities
;; =========================================

(define-read-only (get-metadata)
  (ok { name: CONTRACT-NAME, version: CONTRACT-VERSION, purpose: CONTRACT-PURPOSE }))

(define-read-only (get-params)
  (ok { admin: (var-get admin),
        minDonation: (var-get min-donation),
        withdrawalLimit: (var-get withdrawal-limit),
        requiredSignatures: (var-get required-signatures),
        paused: (var-get paused) }))

(define-read-only (get-stats)
  (ok { donations: (var-get donation-counter),
        withdrawals: (var-get withdrawal-counter),
        balance: (stx-get-balance (as-contract tx-sender)) }))

(define-read-only (can-beneficiary-withdraw (account principal) (amount uint))
  (let ((alloc (default-to u0 (get available (map-get? allocations { account: account })))))
    (ok (and (<= amount alloc) (<= amount (var-get withdrawal-limit)) (not (var-get paused))))))
