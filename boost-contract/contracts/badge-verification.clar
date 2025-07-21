;; Badge Verification Smart Contract - FIXED VERSION
;; Provides on-chain verification of badge authenticity and ownership

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_BADGE_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_RECIPIENT (err u103))
(define-constant ERR_TRANSFER_FAILED (err u104))
(define-constant ERR_INVALID_ISSUER (err u105))

;; Data Variables
(define-data-var next-badge-id uint u1)
(define-data-var contract-uri (string-ascii 256) "")

;; Badge Structure
(define-map badges
  { badge-id: uint }
  {
    owner: principal,
    issuer: principal,
    badge-type: (string-ascii 64),
    title: (string-ascii 128),
    description: (string-ascii 512),
    metadata-uri: (string-ascii 256),
    issued-at: uint,
    expires-at: (optional uint),
    revoked: bool,
    verification-hash: (buff 32)
  }
)

;; Authorized Issuers
(define-map authorized-issuers
  { issuer: principal }
  { 
    name: (string-ascii 128),
    authorized: bool,
    authorized-at: uint
  }
)

;; Badge Ownership History (FIXED - removed transaction-hash)
(define-map ownership-history
  { badge-id: uint, sequence: uint }
  {
    previous-owner: principal,
    new-owner: principal,
    transferred-at: uint
  }
)

;; Badge Type Registry
(define-map badge-types
  { type-name: (string-ascii 64) }
  {
    issuer: principal,
    requirements: (string-ascii 512),
    active: bool,
    created-at: uint
  }
)

;; Verification Requests (for third-party validation)
(define-map verification-requests
  { request-id: (buff 32) }
  {
    requester: principal,
    badge-id: uint,
    verified: bool,
    verified-at: (optional uint),
    verification-data: (string-ascii 256)
  }
)

;; Read-only functions for public verification

;; Get badge details
(define-read-only (get-badge (badge-id uint))
  (map-get? badges { badge-id: badge-id })
)

;; Verify badge ownership
(define-read-only (verify-ownership (badge-id uint) (claimed-owner principal))
  (match (map-get? badges { badge-id: badge-id })
    badge-data (is-eq (get owner badge-data) claimed-owner)
    false
  )
)

;; Verify badge authenticity
(define-read-only (verify-authenticity (badge-id uint))
  (match (map-get? badges { badge-id: badge-id })
    badge-data 
    {
      exists: true,
      owner: (get owner badge-data),
      issuer: (get issuer badge-data),
      revoked: (get revoked badge-data),
      expired: (match (get expires-at badge-data)
        expiry (> stacks-block-height expiry)
        false
      ),
      issuer-authorized: (default-to false (get authorized (map-get? authorized-issuers { issuer: (get issuer badge-data) })))
    }
    { exists: false, owner: CONTRACT_OWNER, issuer: CONTRACT_OWNER, revoked: true, expired: true, issuer-authorized: false }
  )
)

;; Get badge ownership history
(define-read-only (get-ownership-history (badge-id uint) (sequence uint))
  (map-get? ownership-history { badge-id: badge-id, sequence: sequence })
)

;; Check if issuer is authorized
(define-read-only (is-authorized-issuer (issuer principal))
  (default-to false (get authorized (map-get? authorized-issuers { issuer: issuer })))
)

;; Get badge type information
(define-read-only (get-badge-type (type-name (string-ascii 64)))
  (map-get? badge-types { type-name: type-name })
)

;; Verify multiple badges for a user
(define-read-only (verify-user-badges (user principal) (badge-ids (list 10 uint)))
  (map verify-user-badge-ownership badge-ids)
)

(define-private (verify-user-badge-ownership (badge-id uint))
  {
    badge-id: badge-id,
    owned: (verify-ownership badge-id tx-sender),
    authentic: (verify-authenticity badge-id)
  }
)

;; Public functions

;; Authorize an issuer (only contract owner)
(define-public (authorize-issuer (issuer principal) (name (string-ascii 128)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (map-set authorized-issuers
      { issuer: issuer }
      {
        name: name,
        authorized: true,
        authorized-at: stacks-block-height
      }
    ))
  )
)

;; Register a new badge type
(define-public (register-badge-type 
  (type-name (string-ascii 64))
  (requirements (string-ascii 512)))
  (begin
    (asserts! (is-authorized-issuer tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? badge-types { type-name: type-name })) ERR_ALREADY_EXISTS)
    (ok (map-set badge-types
      { type-name: type-name }
      {
        issuer: tx-sender,
        requirements: requirements,
        active: true,
        created-at: stacks-block-height
      }
    ))
  )
)

;; Issue a new badge
(define-public (issue-badge
  (recipient principal)
  (badge-type (string-ascii 64))
  (title (string-ascii 128))
  (description (string-ascii 512))
  (metadata-uri (string-ascii 256))
  (expires-at (optional uint))
  (verification-hash (buff 32)))
  (let ((badge-id (var-get next-badge-id)))
    (begin
      (asserts! (is-authorized-issuer tx-sender) ERR_UNAUTHORIZED)
      (asserts! (not (is-eq recipient CONTRACT_OWNER)) ERR_INVALID_RECIPIENT)
      
      ;; Create the badge
      (map-set badges
        { badge-id: badge-id }
        {
          owner: recipient,
          issuer: tx-sender,
          badge-type: badge-type,
          title: title,
          description: description,
          metadata-uri: metadata-uri,
          issued-at: stacks-block-height,
          expires-at: expires-at,
          revoked: false,
          verification-hash: verification-hash
        }
      )
      
      ;; Increment badge ID counter
      (var-set next-badge-id (+ badge-id u1))
      
      (ok badge-id)
    )
  )
)

;; Transfer badge ownership (FIXED)
(define-public (transfer-badge (badge-id uint) (new-owner principal))
  (match (map-get? badges { badge-id: badge-id })
    badge-data
    (begin
      (asserts! (is-eq tx-sender (get owner badge-data)) ERR_UNAUTHORIZED)
      (asserts! (not (get revoked badge-data)) ERR_TRANSFER_FAILED)
      
      ;; Update badge ownership
      (map-set badges
        { badge-id: badge-id }
        (merge badge-data { owner: new-owner })
      )
      
      ;; Record ownership history (without transaction hash)
      (map-set ownership-history
        { badge-id: badge-id, sequence: stacks-block-height }
        {
          previous-owner: tx-sender,
          new-owner: new-owner,
          transferred-at: stacks-block-height
        }
      )
      
      (ok true)
    )
    ERR_BADGE_NOT_FOUND
  )
)

;; Revoke a badge (only issuer can revoke)
(define-public (revoke-badge (badge-id uint))
  (match (map-get? badges { badge-id: badge-id })
    badge-data
    (begin
      (asserts! (is-eq tx-sender (get issuer badge-data)) ERR_UNAUTHORIZED)
      
      (map-set badges
        { badge-id: badge-id }
        (merge badge-data { revoked: true })
      )
      
      (ok true)
    )
    ERR_BADGE_NOT_FOUND
  )
)

;; Create verification request (for third-party platforms)
(define-public (create-verification-request 
  (request-id (buff 32))
  (badge-id uint)
  (verification-data (string-ascii 256)))
  (begin
    (asserts! (is-some (map-get? badges { badge-id: badge-id })) ERR_BADGE_NOT_FOUND)
    
    (map-set verification-requests
      { request-id: request-id }
      {
        requester: tx-sender,
        badge-id: badge-id,
        verified: true,
        verified-at: (some stacks-block-height),
        verification-data: verification-data
      }
    )
    
    (ok true)
  )
)

;; Get verification request
(define-read-only (get-verification-request (request-id (buff 32)))
  (map-get? verification-requests { request-id: request-id })
)

;; Batch verification for third-party platforms
(define-read-only (batch-verify-badges (badge-ids (list 20 uint)))
  (map get-badge-verification badge-ids)
)

(define-private (get-badge-verification (badge-id uint))
  (let ((badge-data (map-get? badges { badge-id: badge-id })))
    (match badge-data
      data {
        badge-id: badge-id,
        exists: true,
        owner: (get owner data),
        issuer: (get issuer data),
        valid: (and 
          (not (get revoked data))
          (match (get expires-at data)
            expiry (< stacks-block-height expiry)
            true
          )
        ),
        verification-hash: (get verification-hash data)
      }
      {
        badge-id: badge-id,
        exists: false,
        owner: CONTRACT_OWNER,
        issuer: CONTRACT_OWNER,
        valid: false,
        verification-hash: 0x00
      }
    )
  )
)

;; Initialize contract
(define-private (init)
  (begin
    (var-set contract-uri "https://api.badgeverification.com/metadata")
    (map-set authorized-issuers
      { issuer: CONTRACT_OWNER }
      {
        name: "Contract Owner",
        authorized: true,
        authorized-at: stacks-block-height
      }
    )
  )
)

;; Call init on deployment
(init)