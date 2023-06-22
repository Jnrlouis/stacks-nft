;; helloFren
;; <Hello Friends! Wagmi>

(impl-trait 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.nft-trait.nft-trait)

(define-constant contract-owner tx-sender)
(define-constant AMOUNT u100)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-not-found (err u404))
(define-constant err-unsupported-tx (err u500))
(define-constant err-out-not-found (err u501))
(define-constant err-in-not-found (err u502))
(define-constant err-tx-not-mined (err u503))
(define-constant err-address-mismatch (err u504))
(define-constant err-amount-mismatch (err u505))

(define-non-fungible-token fren uint)

(define-data-var last-token-id uint u0)

;; (define-data-var retrievedPrincipal principal tx-sender)
;; (define-data-var retBuff (buff 256) 0xb5463A49BCe74778db3f498900BEba299f5beEa8)


(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok (some "https://ipfs.io/ipfs/QmVnyq3wkdvksobc6hBA1oibWgb8RpnszSxtYg4YG6rxMM"))
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? fren token-id))
)

;; (define-read-only (get-retrieved-principal)
;;     (ok (var-get retrievedPrincipal))
;; )

;; for compressed public keys
(define-read-only (p2pkh-to-principal (scriptSig (buff 256)))
  (let ((pk (unwrap! (as-max-len? (unwrap! (slice? scriptSig (- (len scriptSig) u33) (len scriptSig)) none) u33) none)))
    (some (unwrap! (principal-of? pk) none)))
)


(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (nft-transfer? fren token-id sender recipient)
    )
)

(define-public (mint (recipient principal) (height uint) (tx (buff 1024)) (header (buff 80)) (proof { tx-index: uint, hashes: (list 14 (buff 32)), tree-depth: uint}))
    (let
        (
            (token-id (+ (var-get last-token-id) u1))
            (tx-obj (try! (contract-call? .clarity-bitcoin parse-tx tx)))
            (tx-was-mined (try! (contract-call? .clarity-bitcoin was-tx-mined-compact height tx header proof)))
            ;; (tx-obj (try! (contract-call? 'ST3QFME3CANQFQNR86TYVKQYCFT7QX4PRXM1V9W6H.clarity-bitcoin-bitbadge-v3 parse-tx tx)))
            ;; (tx-was-mined (try! (contract-call? 'ST3QFME3CANQFQNR86TYVKQYCFT7QX4PRXM1V9W6H.clarity-bitcoin-bitbadge-v3 was-tx-mined-compact height tx header proof)))
            (first-output (unwrap! (element-at (get outs tx-obj) u0) err-out-not-found))
            (first-input (unwrap! (element-at (get ins tx-obj) u0) err-in-not-found))
            ;; (retrieved-address (unwrap! (p2pkh-to-principal (get scriptSig first-input)) err-unsupported-tx))
        )
        (asserts! (is-eq tx-was-mined true) err-tx-not-mined)
        (asserts! (is-eq u100 (get value first-output)) err-amount-mismatch)
        ;; (asserts! (is-eq recipient retrieved-address) err-address-mismatch)
        (try! (nft-mint? fren token-id recipient))
        (var-set last-token-id token-id)
        ;; (var-set retrievedPrincipal retrieved-address)
        (ok token-id)
    )
)
