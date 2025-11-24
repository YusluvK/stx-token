;; ---------------------------------------------------------
;; Contract: stx-token.clar
;; Type: SIP-010 compatible fungible token
;; Description: A simple, secure fungible token example
;; ---------------------------------------------------------

;; ----------------------------
;; Constants and Data Variables
;; ----------------------------

(define-constant ERR-NOT-OWNER u100)
(define-constant ERR-ALREADY-INITIALIZED u101)
(define-constant ERR-ZERO-AMOUNT u102)
(define-constant ERR-INSUFFICIENT-BALANCE u103)
(define-constant ERR-OVERFLOW u104)

(define-data-var owner (optional principal) none)
(define-data-var total-supply uint u0)

(define-map balances
  { account: principal }
  { balance: uint })

;; ----------------------------
;; Initialization
;; ----------------------------

(define-public (initialize (token-owner principal))
  (begin
    (asserts! (is-none (var-get owner)) (err ERR-ALREADY-INITIALIZED))
    (var-set owner (some token-owner))
    (ok token-owner)
  )
)

;; ----------------------------
;; Token Information
;; ----------------------------

(define-read-only (get-name) (ok "Stacks Token"))
(define-read-only (get-symbol) (ok "STX"))
(define-read-only (get-decimals) (ok u6)) ;; 6 decimals (e.g. 1.000000 STX)
(define-read-only (get-total-supply) (ok (var-get total-supply)))
(define-read-only (get-owner) (ok (var-get owner)))

;; ----------------------------
;; Balance and Transfer Logic
;; ----------------------------

(define-read-only (get-balance (account principal))
  (match (map-get? balances { account: account })
    entry (ok (get balance entry))
    (ok u0)
  )
)

(define-public (transfer (recipient principal) (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR-ZERO-AMOUNT))
    (let ((sender tx-sender)
          (sender-bal (unwrap! (get-balance sender) (err ERR-INSUFFICIENT-BALANCE)))
          (recipient-bal (unwrap! (get-balance recipient) (err ERR-INSUFFICIENT-BALANCE))))
      (asserts! (>= sender-bal amount) (err ERR-INSUFFICIENT-BALANCE))
      (let ((new-sender-bal (- sender-bal amount))
            (new-recipient-bal (+ recipient-bal amount)))
        (map-set balances { account: sender } { balance: new-sender-bal })
        (map-set balances { account: recipient } { balance: new-recipient-bal })
        (ok true)
      )
    )
  )
)

;; ----------------------------
;; Minting Function
;; ----------------------------

(define-public (mint (recipient principal) (amount uint))
  (let ((current-owner (var-get owner)))
    (if (is-none current-owner)
      (err ERR-ALREADY-INITIALIZED)
      (let ((o (unwrap! current-owner (err ERR-ALREADY-INITIALIZED))))
        (if (is-eq tx-sender o)
          (let ((current-supply (var-get total-supply))
                (recipient-bal (unwrap! (get-balance recipient) (err ERR-INSUFFICIENT-BALANCE))))
            (let ((new-supply (+ current-supply amount))
                  (new-recipient-bal (+ recipient-bal amount)))
              (var-set total-supply new-supply)
              (map-set balances { account: recipient } { balance: new-recipient-bal })
              (ok true)
            )
          )
          (err ERR-NOT-OWNER)
        )
      )
    )
  )
)

;; ----------------------------
;; Burn Function
;; ----------------------------

(define-public (burn (amount uint))
  (let ((sender tx-sender))
    (if (is-eq amount u0)
        (err ERR-ZERO-AMOUNT)
        (let ((sender-bal (unwrap! (get-balance sender) (err ERR-INSUFFICIENT-BALANCE))))
          (if (< sender-bal amount)
              (err ERR-INSUFFICIENT-BALANCE)
              (begin
                (map-set balances { account: sender } { balance: (- sender-bal amount) })
                (var-set total-supply (- (var-get total-supply) amount))
                (ok true)
              )
          )
        )
    )
  )
)
