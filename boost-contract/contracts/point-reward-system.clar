;; Points and Reward Token Management System
;; A comprehensive smart contract for managing user points and reward redemption

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_REWARD_NOT_FOUND (err u103))
(define-constant ERR_REWARD_UNAVAILABLE (err u104))
(define-constant ERR_USER_NOT_FOUND (err u105))

;; Data Variables
(define-data-var total-points-issued uint u0)
(define-data-var next-reward-id uint u1)

;; Data Maps
;; User point balances
(define-map user-balances principal uint)

;; User activity tracking
(define-map user-stats principal {
    total-earned: uint,
    total-spent: uint,
    rewards-redeemed: uint,
    last-activity: uint
})

;; Reward definitions
(define-map rewards uint {
    name: (string-ascii 50),
    description: (string-ascii 200),
    cost: uint,
    available-quantity: uint,
    is-active: bool,
    created-by: principal
})

;; Reward redemption history
(define-map redemption-history {user: principal, reward-id: uint, height: uint} {
    points-spent: uint,
    timestamp: uint
})

;; Admin permissions
(define-map admins principal bool)

;; Private Functions

;; Check if user is admin
(define-private (is-admin (user principal))
    (default-to false (map-get? admins user))
)

;; Get user balance (returns 0 if user not found)
(define-private (get-balance-or-zero (user principal))
    (default-to u0 (map-get? user-balances user))
)

;; Update user stats
(define-private (update-user-stats (user principal) (earned uint) (spent uint))
    (let ((current-stats (default-to 
            {total-earned: u0, total-spent: u0, rewards-redeemed: u0, last-activity: u0}
            (map-get? user-stats user))))
        (map-set user-stats user {
            total-earned: (+ (get total-earned current-stats) earned),
            total-spent: (+ (get total-spent current-stats) spent),
            rewards-redeemed: (get rewards-redeemed current-stats),
            last-activity: stacks-block-height
        })
    )
)

;; Public Functions

;; Initialize contract (set contract owner as admin)
(define-public (initialize)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set admins CONTRACT_OWNER true)
        (ok true)
    )
)

;; Add admin
(define-public (add-admin (new-admin principal))
    (begin
        (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-admin tx-sender)) ERR_UNAUTHORIZED)
        (map-set admins new-admin true)
        (ok true)
    )
)

;; Remove admin
(define-public (remove-admin (admin principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (not (is-eq admin CONTRACT_OWNER)) ERR_UNAUTHORIZED)
        (map-delete admins admin)
        (ok true)
    )
)

;; Award points to user
(define-public (award-points (recipient principal) (amount uint))
    (begin
        (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        
        (let ((current-balance (get-balance-or-zero recipient)))
            (map-set user-balances recipient (+ current-balance amount))
            (var-set total-points-issued (+ (var-get total-points-issued) amount))
            (update-user-stats recipient amount u0)
            (print {
                action: "points-awarded",
                recipient: recipient,
                amount: amount,
                new-balance: (+ current-balance amount)
            })
            (ok amount)
        )
    )
)

;; Deduct points from user (admin only)
(define-public (deduct-points (user principal) (amount uint))
    (begin
        (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        
        (let ((current-balance (get-balance-or-zero user)))
            (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
            (map-set user-balances user (- current-balance amount))
            (update-user-stats user u0 amount)
            (print {
                action: "points-deducted",
                user: user,
                amount: amount,
                new-balance: (- current-balance amount)
            })
            (ok amount)
        )
    )
)

;; Create a new reward
(define-public (create-reward (name (string-ascii 50)) (description (string-ascii 200)) (cost uint) (quantity uint))
    (begin
        (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> cost u0) ERR_INVALID_AMOUNT)
        (asserts! (> quantity u0) ERR_INVALID_AMOUNT)
        
        (let ((reward-id (var-get next-reward-id)))
            (map-set rewards reward-id {
                name: name,
                description: description,
                cost: cost,
                available-quantity: quantity,
                is-active: true,
                created-by: tx-sender
            })
            (var-set next-reward-id (+ reward-id u1))
            (print {
                action: "reward-created",
                reward-id: reward-id,
                name: name,
                cost: cost,
                quantity: quantity
            })
            (ok reward-id)
        )
    )
)

;; Update reward availability
(define-public (update-reward-status (reward-id uint) (is-active bool))
    (begin
        (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
        
        (match (map-get? rewards reward-id)
            reward-data (begin
                (map-set rewards reward-id (merge reward-data {is-active: is-active}))
                (ok true)
            )
            ERR_REWARD_NOT_FOUND
        )
    )
)

;; Redeem reward
(define-public (redeem-reward (reward-id uint))
    (begin
        (let ((user-balance (get-balance-or-zero tx-sender)))
            (match (map-get? rewards reward-id)
                reward-data (begin
                    (asserts! (get is-active reward-data) ERR_REWARD_UNAVAILABLE)
                    (asserts! (> (get available-quantity reward-data) u0) ERR_REWARD_UNAVAILABLE)
                    (asserts! (>= user-balance (get cost reward-data)) ERR_INSUFFICIENT_BALANCE)
                    
                    ;; Deduct points from user
                    (map-set user-balances tx-sender (- user-balance (get cost reward-data)))
                    
                    ;; Reduce available quantity
                    (map-set rewards reward-id (merge reward-data {
                        available-quantity: (- (get available-quantity reward-data) u1)
                    }))
                    
                    ;; Update user stats
                    (let ((current-stats (default-to 
                            {total-earned: u0, total-spent: u0, rewards-redeemed: u0, last-activity: u0}
                            (map-get? user-stats tx-sender))))
                        (map-set user-stats tx-sender (merge current-stats {
                            total-spent: (+ (get total-spent current-stats) (get cost reward-data)),
                            rewards-redeemed: (+ (get rewards-redeemed current-stats) u1),
                            last-activity: stacks-block-height
                        }))
                    )
                    
                    ;; Record redemption
                    (map-set redemption-history 
                        {user: tx-sender, reward-id: reward-id, height: stacks-block-height}
                        {points-spent: (get cost reward-data), timestamp: stacks-block-height}
                    )
                    
                    (print {
                        action: "reward-redeemed",
                        user: tx-sender,
                        reward-id: reward-id,
                        reward-name: (get name reward-data),
                        points-spent: (get cost reward-data),
                        new-balance: (- user-balance (get cost reward-data))
                    })
                    
                    (ok reward-id)
                )
                ERR_REWARD_NOT_FOUND
            )
        )
    )
)

;; Transfer points between users
(define-public (transfer-points (recipient principal) (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (not (is-eq tx-sender recipient)) ERR_INVALID_AMOUNT)
        
        (let ((sender-balance (get-balance-or-zero tx-sender)))
            (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_BALANCE)
            
            (let ((recipient-balance (get-balance-or-zero recipient)))
                (map-set user-balances tx-sender (- sender-balance amount))
                (map-set user-balances recipient (+ recipient-balance amount))
                
                (update-user-stats tx-sender u0 amount)
                (update-user-stats recipient amount u0)
                
                (print {
                    action: "points-transferred",
                    from: tx-sender,
                    to: recipient,
                    amount: amount
                })
                
                (ok amount)
            )
        )
    )
)

;; Read-only Functions

;; Get user balance
(define-read-only (get-user-balance (user principal))
    (ok (get-balance-or-zero user))
)

;; Get user stats
(define-read-only (get-user-stats (user principal))
    (ok (map-get? user-stats user))
)

;; Get reward details
(define-read-only (get-reward (reward-id uint))
    (ok (map-get? rewards reward-id))
)

;; Get total points issued
(define-read-only (get-total-points-issued)
    (ok (var-get total-points-issued))
)

;; Check if user is admin
(define-read-only (check-admin (user principal))
    (ok (is-admin user))
)

;; Get redemption history
(define-read-only (get-redemption (user principal) (reward-id uint) (height uint))
    (ok (map-get? redemption-history {user: user, reward-id: reward-id, height: stacks-block-height}))
)

;; Get next reward ID
(define-read-only (get-next-reward-id)
    (ok (var-get next-reward-id))
)
