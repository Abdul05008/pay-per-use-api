;; pay-per-use-api.clar
;; Pay-per-use API access contract (STX)
;; @contract-disable-checked-data true

(define-constant ERR_NOT_PROVIDER u100)
(define-constant ERR_PLAN_NOT_FOUND u101)
(define-constant ERR_PLAN_INACTIVE u102)
(define-constant ERR_INVALID_AMOUNT u103)
(define-constant ERR_TRANSFER_FAIL u104)
(define-constant ERR_NO_FUNDS u105)
(define-constant ERR_NOT_SUBSCRIBED u106)
(define-constant ERR_NO_CREDITS u107)
(define-constant ERR_ONLY_ADMIN u108)
(define-constant ERR_INVALID_INPUT u109)
(define-constant ERR_UNAUTHORIZED u110)

;; Admin (for optional global controls)
(define-data-var admin principal tx-sender)

;; Helper functions for validation
(define-private (check-uint (value uint))
  (if (> value u0)
    (ok value)
    (err ERR_INVALID_AMOUNT)))

(define-private (check-string-not-empty (value (string-ascii 64)))
  (if (> (len value) u0)
    (ok value)
    (err ERR_INVALID_INPUT)))

(define-private (check-admin)
  (if (is-eq tx-sender (var-get admin))
    (ok true)
    (err ERR_ONLY_ADMIN)))

(define-private (check-owner (owner principal))
  (if (is-eq tx-sender owner)
    (ok true)
    (err ERR_UNAUTHORIZED)))

(define-private (check-and-get-plan (plan-id uint))
  (match (map-get? plans { plan-id: plan-id })
    plan (if (get active plan)
          (ok {
            price: (get price plan),
            provider: (get provider plan),
            name: (get name plan),
            active: (get active plan)
          })
          (err ERR_PLAN_INACTIVE))
    (err ERR_PLAN_NOT_FOUND)))



(define-private (get-safe-balance (user principal))
  (default-to u0 (get balance (map-get? provider-balances { who: user }))))

(define-private (get-safe-credits (plan-id uint) (user principal))
  (default-to u0 (get credits (map-get? credits { plan-id: plan-id, who: user }))))

;; Plan counter
(define-data-var next-plan-id uint u1)

;; Plans: plan-id -> { provider: principal, name: (string-ascii 64), price: uint (microSTX), active: bool }
(define-map plans
  { plan-id: uint }
  { provider: principal, name: (string-ascii 64), price: uint, active: bool })

;; Provider balances (microSTX) accumulated in contract from pay-per-call
(define-map provider-balances
  { who: principal } { balance: uint })

;; Usage record counter (for indexing)
(define-data-var next-usage-id uint u1)

;; Usages: usage-id -> { consumer: principal, plan-id: uint, provider: principal, price: uint, block: uint, metadata: (optional (string-ascii 256)) }
(define-map usages
  { usage-id: uint }
  { consumer: principal, plan-id: uint, provider: principal, price: uint, block: uint, metadata: (optional (string-ascii 256)) })

;; Prepaid credits: { plan-id, who } -> credits (uint)
(define-map credits
  { plan-id: uint, who: principal } { credits: uint })

;; --------- Remove event functions and use inline print instead --------

;; -------------------------
;; Admin / provider functions
;; -------------------------

(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_ONLY_ADMIN))
    (var-set admin new-admin)
    (ok true)))

;; Create a new API plan: provider creates for themselves
(define-public (create-plan (name (string-ascii 64)) (price uint))
  (begin
    ;; Validate inputs
    (asserts! (and (> (len name) u0) (> price u0)) (err ERR_INVALID_INPUT))
    
    ;; Create plan
    (let ((plan-id (var-get next-plan-id)))
      (begin
        (var-set next-plan-id (+ plan-id u1))
        (map-set plans 
          { plan-id: plan-id }
          {
            provider: tx-sender,
            name: name,
            price: price,
            active: true
          })
        (print {
          event: "plan-created",
          plan_id: plan-id,
          provider: tx-sender,
          name: name,
          price: price
        })
        (ok plan-id)))));; Provider updates their plan (price or active flag)
(define-public (update-plan (plan-id uint) (price uint) (active bool))
  (let ((p? (map-get? plans { plan-id: plan-id })))
    (asserts! (is-some p?) (err ERR_PLAN_NOT_FOUND))
    (let ((p (unwrap-panic p?)))
      (asserts! (is-eq tx-sender (get provider p)) (err ERR_NOT_PROVIDER))
      (asserts! (> price u0) (err ERR_INVALID_AMOUNT))
      (let ((provider-val (get provider p))
            (name-val (get name p)))
        (map-set plans { plan-id: plan-id } { provider: provider-val, name: name-val, price: price, active: active })
        (print { event: "plan-updated", plan_id: plan-id, price: price, active: active })
        (ok true)))))

;; -------------------------
;; Consumer flows
;; -------------------------

;; Pay per call: consumer pays price (microSTX) to contract in the same call,
;; contract credits provider balance and emits a usage event that off-chain gateway watches.
;; metadata optional (e.g., request id, endpoint params hash) to help gateway correlate payment -> request.
(define-public (pay-and-call (plan-id uint) (metadata (optional (string-ascii 256))))
  (let ((p? (map-get? plans { plan-id: plan-id })))
    (asserts! (is-some p?) (err ERR_PLAN_NOT_FOUND))
    (let ((p (unwrap-panic p?)))
      (asserts! (get active p) (err ERR_PLAN_INACTIVE))
      (let ((price (get price p))
            (provider (get provider p)))
        (asserts! (> price u0) (err ERR_INVALID_AMOUNT))
          ;; transfer price from consumer to contract
        (unwrap! (stx-transfer? price tx-sender contract-caller) (err ERR_TRANSFER_FAIL))
        ;; credit provider balance
        (let ((oldb (default-to u0 (get balance (map-get? provider-balances { who: provider })))))
          (map-set provider-balances { who: provider } { balance: (+ oldb price) }))
        ;; record usage
        (let ((uid (var-get next-usage-id)))
          (var-set next-usage-id (+ uid u1))
          (map-set usages { usage-id: uid } { consumer: tx-sender, plan-id: plan-id, provider: provider, price: price, block: u0, metadata: metadata })
          (print { event: "pay-and-call", usage_id: uid, plan_id: plan-id })
          (ok uid))))))

;; Prepay credits for a plan (buy N credits): buyer transfers (price * credits) microSTX into contract
(define-public (buy-credits (plan-id uint) (num-credits uint))
  (begin
    ;; Input validation
    (asserts! (> num-credits u0) (err ERR_INVALID_AMOUNT))
    
    ;; Get and validate plan
    (match (map-get? plans { plan-id: plan-id })
      p (begin
          ;; Validate plan is active
          (asserts! (get active p) (err ERR_PLAN_INACTIVE))
          
          ;; Calculate cost
          (let ((price (get price p))
                (provider (get provider p))
                (total (* price num-credits)))
            
            ;; Validate payment
            (asserts! (> total u0) (err ERR_INVALID_AMOUNT))
            ;; transfer total from buyer to contract
            (unwrap! (stx-transfer? total tx-sender contract-caller) (err ERR_TRANSFER_FAIL))
            
            ;; Update provider balance
            (let ((oldb (default-to u0 (get balance (map-get? provider-balances { who: provider })))))
              (map-set provider-balances { who: provider } { balance: (+ oldb total) }))
            
            ;; Update credits and return
            (let ((oldc (default-to u0 (get credits (map-get? credits { plan-id: plan-id, who: tx-sender })))))
              (map-set credits { plan-id: plan-id, who: tx-sender } { credits: (+ oldc num-credits) })
              (print { event: "buy-credits", plan_id: plan-id })
              (ok (+ oldc num-credits)))))
      (err ERR_PLAN_NOT_FOUND))));; Consume a prepaid credit (no STX required at consumption time). Emits a usage event for gateway.
(define-public (consume-credit (plan-id uint) (metadata (optional (string-ascii 256))))
  (let ((c? (map-get? credits { plan-id: plan-id, who: tx-sender })))
    (asserts! (is-some c?) (err ERR_NO_CREDITS))
    (let ((rec (unwrap-panic c?))
          (p? (map-get? plans { plan-id: plan-id })))
      (asserts! (is-some p?) (err ERR_PLAN_NOT_FOUND))
      (let ((p (unwrap-panic p?)))
        (asserts! (get active p) (err ERR_PLAN_INACTIVE))
        (let ((cur (get credits rec)))
          (asserts! (> cur u0) (err ERR_NO_CREDITS))
          (let ((new (- cur u1)))
            (map-set credits { plan-id: plan-id, who: tx-sender } { credits: new })
            ;; create usage event -- note: provider was already credited at purchase-time
            (let ((uid (var-get next-usage-id)))
              (var-set next-usage-id (+ uid u1))
              (map-set usages { usage-id: uid } { consumer: tx-sender, plan-id: plan-id, provider: (get provider p), price: u0, block: u0, metadata: metadata })
              (print { event: "consume-credit", plan_id: plan-id })
              (ok uid))))))))

;; -------------------------
;; Provider withdrawals
;; -------------------------
(define-public (withdraw-earnings (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
    (match (map-get? provider-balances { who: tx-sender })
      balance-map (let ((balance (get balance balance-map)))
                    (asserts! (>= balance amount) (err ERR_NO_FUNDS))
                    ;; zero out or subtract before transfer
                    (map-set provider-balances { who: tx-sender } { balance: (- balance amount) })
                    ;; transfer from contract to provider
                    (as-contract
                      (begin
                        (try! (stx-transfer? amount tx-sender tx-sender))
                        (print { event: "withdraw" })
                        (ok true))))
      (err ERR_NO_FUNDS))))

;; -------------------------
;; Read-only views
;; -------------------------
(define-read-only (get-plan (plan-id uint))
  (ok (map-get? plans { plan-id: plan-id })))

(define-read-only (get-provider-balance (who principal))
  (ok (get-safe-balance who)))

(define-read-only (get-usage (usage-id uint))
  (ok (map-get? usages { usage-id: usage-id })))

(define-read-only (get-credits (plan-id uint) (who principal))
  (ok (default-to u0 (get credits (map-get? credits { plan-id: plan-id, who: who })))))

(define-read-only (get-next-plan-id) (ok (var-get next-plan-id)))
(define-read-only (get-next-usage-id) (ok (var-get next-usage-id)))
