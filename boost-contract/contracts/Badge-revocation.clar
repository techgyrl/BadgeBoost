;; Badge Revocation and Expiry Smart Contract
;; Manages badge lifecycle including revocation and expiration

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-BADGE-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-REVOKED (err u102))
(define-constant ERR-INVALID-EXPIRY-TIME (err u103))
(define-constant ERR-BADGE-EXPIRED (err u104))
(define-constant ERR-CANNOT-REVOKE-EXPIRED (err u105))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures
(define-map badges
  { badge-id: uint }
  {
    recipient: principal,
    issuer: principal,
    issued-at: uint,
    expires-at: (optional uint),
    revoked: bool,
    revoked-at: (optional uint),
    revoked-by: (optional principal),
    revocation-reason: (optional (string-ascii 256))
  }
)

(define-map authorized-administrators
  { admin: principal }
  { authorized: bool }
)

(define-map badge-metadata
  { badge-id: uint }
  {
    badge-type: (string-ascii 64),
    title: (string-ascii 128),
    description: (string-ascii 512),
    criteria: (string-ascii 256)
  }
)

;; Badge counter
(define-data-var badge-counter uint u0)

;; Authorization functions
(define-public (add-administrator (admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-administrators { admin: admin } { authorized: true }))
  )
)

(define-public (remove-administrator (admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-administrators { admin: admin } { authorized: false }))
  )
)

(define-read-only (is-authorized (admin principal))
  (default-to false 
    (get authorized (map-get? authorized-administrators { admin: admin }))
  )
)

;; Badge issuance function
(define-public (issue-badge 
  (recipient principal)
  (badge-type (string-ascii 64))
  (title (string-ascii 128))
  (description (string-ascii 512))
  (criteria (string-ascii 256))
  (expires-at (optional uint)))
  (let
    (
      (badge-id (+ (var-get badge-counter) u1))
      (current-time stacks-block-height)
    )
    ;; Check if issuer is authorized
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-authorized tx-sender)) ERR-NOT-AUTHORIZED)
    
    ;; Validate expiry time if provided
    (if (is-some expires-at)
      (asserts! (> (unwrap-panic expires-at) current-time) ERR-INVALID-EXPIRY-TIME)
      true
    )
    
    ;; Store badge data
    (map-set badges
      { badge-id: badge-id }
      {
        recipient: recipient,
        issuer: tx-sender,
        issued-at: current-time,
        expires-at: expires-at,
        revoked: false,
        revoked-at: none,
        revoked-by: none,
        revocation-reason: none
      }
    )
    
    ;; Store badge metadata
    (map-set badge-metadata
      { badge-id: badge-id }
      {
        badge-type: badge-type,
        title: title,
        description: description,
        criteria: criteria
      }
    )
    
    ;; Update counter
    (var-set badge-counter badge-id)
    (ok badge-id)
  )
)

;; Badge revocation function
(define-public (revoke-badge 
  (badge-id uint) 
  (reason (string-ascii 256)))
  (let
    (
      (badge-data (unwrap! (map-get? badges { badge-id: badge-id }) ERR-BADGE-NOT-FOUND))
      (current-time stacks-block-height)
    )
    ;; Check authorization
    (asserts! (or 
      (is-eq tx-sender CONTRACT-OWNER)
      (is-authorized tx-sender)
      (is-eq tx-sender (get issuer badge-data))
    ) ERR-NOT-AUTHORIZED)
    
    ;; Check if badge is not already revoked
    (asserts! (not (get revoked badge-data)) ERR-ALREADY-REVOKED)
    
    ;; Check if badge is not expired (can't revoke expired badges)
    (if (is-some (get expires-at badge-data))
      (asserts! (< current-time (unwrap-panic (get expires-at badge-data))) ERR-CANNOT-REVOKE-EXPIRED)
      true
    )
    
    ;; Update badge with revocation data
    (map-set badges
      { badge-id: badge-id }
      (merge badge-data {
        revoked: true,
        revoked-at: (some current-time),
        revoked-by: (some tx-sender),
        revocation-reason: (some reason)
      })
    )
    
    (ok true)
  )
)

;; Check if badge is valid (not revoked and not expired)
(define-read-only (is-badge-valid (badge-id uint))
  (let
    (
      (badge-data (unwrap! (map-get? badges { badge-id: badge-id }) ERR-BADGE-NOT-FOUND))
      (current-time stacks-block-height)
    )
    ;; Check if revoked
    (if (get revoked badge-data)
      (ok false)
      ;; Check if expired
      (if (is-some (get expires-at badge-data))
        (ok (< current-time (unwrap-panic (get expires-at badge-data))))
        (ok true)
      )
    )
  )
)

;; Check if badge is expired
(define-read-only (is-badge-expired (badge-id uint))
  (let
    (
      (badge-data (unwrap! (map-get? badges { badge-id: badge-id }) ERR-BADGE-NOT-FOUND))
      (current-time stacks-block-height)
    )
    (if (is-some (get expires-at badge-data))
      (ok (>= current-time (unwrap-panic (get expires-at badge-data))))
      (ok false) ;; Badges without expiry don't expire
    )
  )
)

;; Get badge information
(define-read-only (get-badge (badge-id uint))
  (ok (map-get? badges { badge-id: badge-id }))
)

;; Get badge metadata
(define-read-only (get-badge-metadata (badge-id uint))
  (ok (map-get? badge-metadata { badge-id: badge-id }))
)

;; Get complete badge information (combines badge data and metadata)
(define-read-only (get-complete-badge-info (badge-id uint))
  (let
    (
      (badge-data (unwrap! (map-get? badges { badge-id: badge-id }) ERR-BADGE-NOT-FOUND))
      (metadata (unwrap! (map-get? badge-metadata { badge-id: badge-id }) ERR-BADGE-NOT-FOUND))
      (is-valid (unwrap! (is-badge-valid badge-id) ERR-BADGE-NOT-FOUND))
      (is-expired (unwrap! (is-badge-expired badge-id) ERR-BADGE-NOT-FOUND))
    )
    (ok {
      badge-data: badge-data,
      metadata: metadata,
      is-valid: is-valid,
      is-expired: is-expired
    })
  )
)

;; Get badges by recipient
(define-read-only (get-recipient-badges (recipient principal))
  ;; Note: This is a simplified version. In a production contract,
  ;; you'd want to maintain a separate map for efficient querying
  (ok "Use get-complete-badge-info with specific badge IDs")
)

;; Update badge expiry (only for non-expired, non-revoked badges)
(define-public (update-badge-expiry 
  (badge-id uint) 
  (new-expires-at (optional uint)))
  (let
    (
      (badge-data (unwrap! (map-get? badges { badge-id: badge-id }) ERR-BADGE-NOT-FOUND))
      (current-time stacks-block-height)
    )
    ;; Check authorization (only issuer or admin can update)
    (asserts! (or 
      (is-eq tx-sender CONTRACT-OWNER)
      (is-authorized tx-sender)
      (is-eq tx-sender (get issuer badge-data))
    ) ERR-NOT-AUTHORIZED)
    
    ;; Check badge is not revoked
    (asserts! (not (get revoked badge-data)) ERR-ALREADY-REVOKED)
    
    ;; Check badge is not expired
    (if (is-some (get expires-at badge-data))
      (asserts! (< current-time (unwrap-panic (get expires-at badge-data))) ERR-BADGE-EXPIRED)
      true
    )
    
    ;; Validate new expiry time if provided
    (if (is-some new-expires-at)
      (asserts! (> (unwrap-panic new-expires-at) current-time) ERR-INVALID-EXPIRY-TIME)
      true
    )
    
    ;; Update badge expiry
    (map-set badges
      { badge-id: badge-id }
      (merge badge-data {
        expires-at: new-expires-at
      })
    )
    
    (ok true)
  )
)

;; Batch revocation function
(define-public (batch-revoke-badges 
  (badge-ids (list 10 uint)) 
  (reason (string-ascii 256)))
  (begin
    ;; Check authorization
    (asserts! (or 
      (is-eq tx-sender CONTRACT-OWNER)
      (is-authorized tx-sender)
    ) ERR-NOT-AUTHORIZED)
    
    (ok (map revoke-single-badge badge-ids))
  )
)

(define-private (revoke-single-badge (badge-id uint))
  (match (revoke-badge badge-id "Batch revocation")
    success true
    error false
  )
)

;; Emergency functions (contract owner only)
(define-public (emergency-revoke-all-badges-by-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    ;; This would require additional data structures to efficiently implement
    ;; For now, return success - implementation would depend on indexing strategy
    (ok "Emergency revocation initiated - requires additional implementation")
  )
)

;; Get current badge counter
(define-read-only (get-badge-counter)
  (ok (var-get badge-counter))
)