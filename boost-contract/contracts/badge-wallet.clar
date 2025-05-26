;; Digital Badge Wallet Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-badge-exists (err u101))
(define-constant err-badge-not-found (err u102))
(define-constant err-issuer-exists (err u103))
(define-constant err-issuer-not-found (err u104))
(define-constant err-not-badge-owner (err u105))
(define-constant err-not-badge-issuer (err u106))
(define-constant err-badge-revoked (err u107))
(define-constant err-badge-already-issued (err u108))
(define-constant err-invalid-expiration (err u109))
(define-constant err-badge-expired (err u110))

;; Define data maps
(define-map badge-types
  { id: uint }
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    issuer: principal,
    metadata: (string-ascii 256),
    image-uri: (string-ascii 256),
    created-at: uint,
    total-issued: uint,
    active: bool
  }
)

(define-map badge-instances
  { id: uint }
  {
    badge-type-id: uint,
    owner: principal,
    issuer: principal,
    issued-at: uint,
    expiration: (optional uint),
    revoked: bool,
    revoked-at: (optional uint),
    verification-hash: (buff 32)
  }
)

(define-map authorized-issuers
  { address: principal }
  {
    name: (string-ascii 64),
    website: (string-ascii 128),
    verified: bool,
    badges-created: uint,
    badges-issued: uint,
    created-at: uint
  }
)

(define-map user-badges
  { user: principal, badge-instance-id: uint }
  {
    display-order: uint
  }
)

(define-map badge-metadata
  { badge-instance-id: uint }
  {
    attributes: (list 10 {name: (string-ascii 32), value: (string-ascii 64)}),
    achievements: (list 10 (string-ascii 128)),
    custom-data: (string-ascii 256)
  }
)

;; Define variables
(define-data-var badge-type-counter uint u0)
(define-data-var badge-instance-counter uint u0)

;; Register as an authorized issuer
(define-public (register-issuer (name (string-ascii 64)) (website (string-ascii 128)))
  (let
    (
      (existing-issuer (map-get? authorized-issuers { address: tx-sender }))
    )
    (asserts! (is-none existing-issuer) (err err-issuer-exists))
    
    (map-set authorized-issuers
      { address: tx-sender }
      {
        name: name,
        website: website,
        verified: false,
        badges-created: u0,
        badges-issued: u0,
        created-at: stacks-block-height
      }
    )
    
    (ok true)
  )
)

;; Verify an issuer (only contract owner)
(define-public (verify-issuer (issuer-address principal))
  (let
    (
      (issuer (unwrap! (map-get? authorized-issuers { address: issuer-address }) (err err-issuer-not-found)))
    )
    (asserts! (is-eq tx-sender contract-owner) (err err-not-authorized))
    
    (map-set authorized-issuers
      { address: issuer-address }
      (merge issuer { verified: true })
    )
    
    (ok true)
  )
)

;; Create a new badge type
(define-public (create-badge-type (name (string-ascii 64)) (description (string-ascii 256)) (metadata (string-ascii 256)) (image-uri (string-ascii 256)))
  (let
    (
      (issuer (unwrap! (map-get? authorized-issuers { address: tx-sender }) (err err-not-authorized)))
      (badge-type-id (+ (var-get badge-type-counter) u1))
    )
    
    (map-set badge-types
      { id: badge-type-id }
      {
        name: name,
        description: description,
        issuer: tx-sender,
        metadata: metadata,
        image-uri: image-uri,
        created-at: stacks-block-height,
        total-issued: u0,
        active: true
      }
    )
    
    ;; Update issuer stats
    (map-set authorized-issuers
      { address: tx-sender }
      (merge issuer {
        badges-created: (+ (get badges-created issuer) u1)
      })
    )
    
    (var-set badge-type-counter badge-type-id)
    (ok badge-type-id)
  )
)
