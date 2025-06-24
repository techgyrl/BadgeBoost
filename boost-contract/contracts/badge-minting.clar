;; Badge Minting Smart Contract
;; Implements NFT-based achievement badges with Open Badges compliance

;; Define the NFT
(define-non-fungible-token achievement-badge uint)

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-BADGE-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-MINTED (err u102))
(define-constant ERR-INVALID-ISSUER (err u103))
(define-constant ERR-INVALID-METADATA (err u104))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures for Open Badges compliance
(define-map badge-metadata
    uint
    {
        name: (string-ascii 64),
        description: (string-ascii 256),
        image: (string-ascii 256),
        criteria: (string-ascii 256),
        issuer-name: (string-ascii 64),
        issuer-url: (string-ascii 256),
        tags: (list 10 (string-ascii 32)),
        skill-level: (string-ascii 16),
        expires-on: (optional uint)
    }
)

;; Track badge recipients and issuance details
(define-map badge-recipients
    uint
    {
        recipient: principal,
        earned-on: uint,
        evidence-url: (optional (string-ascii 256)),
        verification-hash: (string-ascii 64)
    }
)

;; Authorized badge issuers
(define-map authorized-issuers principal bool)

;; Badge counter for unique IDs
(define-data-var badge-counter uint u0)

;; Track badges by recipient for easy querying
(define-map recipient-badges principal (list 100 uint))

;; Initialize contract owner as authorized issuer
(map-set authorized-issuers CONTRACT-OWNER true)

;; Public functions

;; Authorize a new badge issuer (only contract owner)
(define-public (authorize-issuer (issuer principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok (map-set authorized-issuers issuer true))
    )
)

;; Revoke issuer authorization (only contract owner)
(define-public (revoke-issuer (issuer principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok (map-set authorized-issuers issuer false))
    )
)

;; Mint a new achievement badge
(define-public (mint-badge 
    (recipient principal)
    (name (string-ascii 64))
    (description (string-ascii 256))
    (image (string-ascii 256))
    (criteria (string-ascii 256))
    (issuer-name (string-ascii 64))
    (issuer-url (string-ascii 256))
    (tags (list 10 (string-ascii 32)))
    (skill-level (string-ascii 16))
    (expires-on (optional uint))
    (evidence-url (optional (string-ascii 256)))
    (verification-hash (string-ascii 64))
)
    (let 
        (
            (badge-id (+ (var-get badge-counter) u1))
            ;; (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        )
        ;; Check if sender is authorized issuer
        (asserts! (default-to false (map-get? authorized-issuers tx-sender)) ERR-NOT-AUTHORIZED)
        
        ;; Validate required fields
        (asserts! (> (len name) u0) ERR-INVALID-METADATA)
        (asserts! (> (len description) u0) ERR-INVALID-METADATA)
        (asserts! (> (len verification-hash) u0) ERR-INVALID-METADATA)
        
        ;; Mint the NFT
        (try! (nft-mint? achievement-badge badge-id recipient))
        
        ;; Store badge metadata (Open Badges compliant)
        (map-set badge-metadata badge-id {
            name: name,
            description: description,
            image: image,
            criteria: criteria,
            issuer-name: issuer-name,
            issuer-url: issuer-url,
            tags: tags,
            skill-level: skill-level,
            expires-on: expires-on
        })
        
        ;; Store recipient details
        (map-set badge-recipients badge-id {
            recipient: recipient,
            earned-on: stacks-block-height,
            evidence-url: evidence-url,
            verification-hash: verification-hash
        })
        
        ;; Update recipient's badge list
        (let 
            (
                (current-badges (default-to (list) (map-get? recipient-badges recipient)))
            )
            (map-set recipient-badges recipient (unwrap-panic (as-max-len? (append current-badges badge-id) u100)))
        )
        
        ;; Increment badge counter
        (var-set badge-counter badge-id)
        
        (ok badge-id)
    )
)

;; Transfer badge (with restrictions for achievement badges)
;; (define-public (transfer-badge (badge-id uint) (sender principal) (recipient principal))
;;     (begin
;;         ;; Only badge owner can transfer
;;         (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
        
;;         ;; Check if badge exists and sender owns it
;;         (asserts! (is-eq (some sender) (nft-get-owner? achievement-badge badge-id)) ERR-NOT-AUTHORIZED)
        
;;         ;; Transfer the NFT
;;         (try! (nft-transfer? achievement-badge badge-id sender recipient))
        
;;         ;; Update recipient tracking
;;         (match (map-get? badge-recipients badge-id)
;;             badge-info 
;;             (map-set badge-recipients badge-id (merge badge-info { recipient: recipient }))
;;             (err ERR-BADGE-NOT-FOUND)
;;         )
        
;;         ;; Update badge lists for both sender and recipient
;;         (let 
;;             (
;;                 (sender-badges (filter-badge-from-list 
;;                     (default-to (list) (map-get? recipient-badges sender)) 
;;                     badge-id))
;;                 (recipient-badges-list (default-to (list) (map-get? recipient-badges recipient)))
;;             )
;;             (map-set recipient-badges sender sender-badges)
;;             (map-set recipient-badges recipient 
;;                 (unwrap-panic (as-max-len? (append recipient-badges-list badge-id) u100)))
;;         )
        
;;         (ok true)
;;     )
;; )

;; Read-only functions

;; Get badge metadata
(define-read-only (get-badge-metadata (badge-id uint))
    (map-get? badge-metadata badge-id)
)

;; Get badge recipient info
(define-read-only (get-badge-recipient (badge-id uint))
    (map-get? badge-recipients badge-id)
)

;; Get badge owner
(define-read-only (get-badge-owner (badge-id uint))
    (nft-get-owner? achievement-badge badge-id)
)

;; Get all badges for a recipient
(define-read-only (get-recipient-badges (recipient principal))
    (map-get? recipient-badges recipient)
)

;; Check if principal is authorized issuer
(define-read-only (is-authorized-issuer (issuer principal))
    (default-to false (map-get? authorized-issuers issuer))
)

;; Get current badge counter
(define-read-only (get-badge-counter)
    (var-get badge-counter)
)

;; Get complete badge information (metadata + recipient info)
(define-read-only (get-full-badge-info (badge-id uint))
    (let 
        (
            (metadata (map-get? badge-metadata badge-id))
            (recipient-info (map-get? badge-recipients badge-id))
            (owner (nft-get-owner? achievement-badge badge-id))
        )
        {
            metadata: metadata,
            recipient-info: recipient-info,
            current-owner: owner
        }
    )
)

;; Private helper functions

;; Filter a specific badge ID from a list
(define-private (filter-badge-from-list (badge-list (list 100 uint)) (badge-to-remove uint))
    (filter not-equal-to-badge badge-list)
)

;; Helper function for filtering
(define-private (not-equal-to-badge (badge-id uint))
    (not (is-eq badge-id badge-id))
)

;; Validate Open Badges compliance
(define-private (validate-open-badges-metadata 
    (name (string-ascii 64))
    (description (string-ascii 256))
    (criteria (string-ascii 256))
    (issuer-name (string-ascii 64))
)
    (and 
        (> (len name) u0)
        (> (len description) u0)
        (> (len criteria) u0)
        (> (len issuer-name) u0)
    )
)