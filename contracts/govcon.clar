;; Governance contract for managing the blockchain relief fund

;; =========================================
;; Data variables
;; =========================================

(define-data-var admin principal tx-sender)
(define-data-var min-donation uint u10)
(define-data-var withdrawal-limit uint u1000)
(define-data-var required-signatures uint u3)
(define-data-var tx-nonce uint u0)


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


;; Restrictions for disaster relief
;; =========================================

(define-public (validate-donation (amount uint))
 (if (>= amount (var-get min-donation)) (ok true) (err u404)))

(define-public (validate-withdrawal (amount uint))
 (if (<= amount (var-get withdrawal-limit)) (ok true) (err u405)))
