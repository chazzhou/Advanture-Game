;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname adventure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "adventure-define-struct.rkt")
(require "macros.rkt")
(require "utilities.rkt")
(require racket/bool)

(define (counter) 0)
(define (c++) (set! counter (+ counter 1)))
(define (czero) (set! counter 0))
;;;
;;; OBJECT
;;; Base type for all in-game objects
;;;

(define-struct object
  ;; adjectives: (listof string)
  ;; List of adjectives to be printed in the description of this object
  (adjectives)
  
  #:methods
  ;; noun: object -> string
  ;; Returns the noun to use to describe this object.
  (define (noun o)
    (type-name-string o))

  ;; description-word-list: object -> (listof string)
  ;; The description of the object as a list of individual
  ;; words, e.g. '("a" "red" "door").
  (define (description-word-list o)
    (add-a-or-an (append (object-adjectives o)
                         (list (noun o)))))
  ;; description: object -> string
  ;; Generates a description of the object as a noun phrase, e.g. "a red door".
  (define (description o)
    (words->string (description-word-list o)))
  
  ;; print-description: object -> void
  ;; EFFECT: Prints the description of the object.
  (define (print-description o)
    (begin (printf (description o))
           (newline)
           (void)))
  ;; other type methods are defined here so a specified message can be displayed easily if the method is called on a type it shouldn't be
  (define (eat food)
    (display-line "Funny. That isn't food."))
  (define (drink o)
    (display-line "lol. That isn't a beverage."))
  (define (open x)
    (begin
      (display-line "That object isn't a storage container")))
  (define (wear x)
    (display-line "You can't wear that."))
  (define (charge laptop)
    (display-line "You don't know how to charge that.")))



;;;
;;; CONTAINER
;;; Base type for all game objects that can hold things
;;;

(define-struct (container object)
  ;; contents: (listof thing)
  ;; List of things presently in this container
  (contents)
  
  #:methods
  ;; container-accessible-contents: container -> (listof thing)
  ;; Returns the objects from the container that would be accessible to the player.
  ;; By default, this is all the objects.  But if you want to implement locked boxes,
  ;; rooms without light, etc., you can redefine this to withhold the contents under
  ;; whatever conditions you like.
  (define (container-accessible-contents c)
    (container-contents c))
  
  ;; prepare-to-remove!: container thing -> void
  ;; Called by move when preparing to move thing out of
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-remove! container thing)
    (void))
  
  ;; prepare-to-add!: container thing -> void
  ;; Called by move when preparing to move thing into
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-add! container thing)
    (void))
  
  ;; remove!: container thing -> void
  ;; EFFECT: removes the thing from the container
  (define (remove! container thing)
    (set-container-contents! container
                             (remove thing
                                     (container-contents container)))))
  
  ;; add!: container thing -> void
  ;; EFFECT: adds the thing to the container.  Does not update the thing's location.
  (define (add! container thing)
    (set-container-contents! container
                             (cons thing
                                   (container-contents container))))

  ;; describe-contents: container -> void
  ;; EFFECT: prints the contents of the container
  (define (describe-contents container)
    (begin (local [(define other-stuff (remove me (container-accessible-contents container)))]
             (if (empty? other-stuff)
                 (printf "There's nothing here.~%")
                 (begin (printf "You see:~%")
                        (for-each print-description other-stuff))))
           (void))) 

;; move!: thing container -> void
;; Moves thing from its previous location to container.
;; EFFECT: updates location field of thing and contents
;; fields of both the new and old containers.
(define (move! thing new-container)
  (begin
    (prepare-to-remove! (thing-location thing)
                        thing)
    (prepare-to-add! new-container thing)
    (prepare-to-move! thing new-container)
    (remove! (thing-location thing)
             thing)
    (add! new-container thing)
    (set-thing-location! thing new-container)))

;; destroy!: thing -> void
;; EFFECT: removes thing from the game completely.
(define (destroy! thing)
  ; We just remove it from its current location
  ; without adding it anyplace else.
  (remove! (thing-location thing)
           thing))

;;;
;;; ROOM
;;; Base type for rooms and outdoor areas
;;;

(define-struct (room container)
  (;; trap: boolean
   ;; If the room behind it is trap.
   trap)
  ;; hide the noun.
  #:methods
  ;; description-word-list: object -> (listof string)
  ;; The description of the object as a list of individual
  ;; words, e.g. '("a" "red" "door").
  (define (description-word-list o)
    (add-a-or-an (object-adjectives o)))
  )

;; new-room: string -> room
;; Makes a new room with the specified adjectives
 (define (new-room adjectives)
  (make-room (string->words adjectives)
             '()
             #false))

;;;
;;; THING
;;; Base type for all physical objects that can be inside other objects such as rooms
;;;

(define-struct (thing container)
  ;; location: container
  ;; What room or other container this thing is presently located in.
  (location
   takable?)
  
  #:methods
  ;; take: thing -> void
  ;; EFFECT: .
  (define (take thing)
  (begin
    (if (eq? (thing-location thing) me)
        (display-line "You took the item already.")
        (if (thing-takable? thing)
            (begin
              (update-stats 1 1)
              (move! thing me)
              (display-line "You took the item.")
              (sleep 1)
              (look))
            (display-line "You can't take that.")))))
  
    (define (examine thing)
      (print-description thing))

    ;; prepare-to-move!: thing container -> void
    ;; Called by move when preparing to move thing into
    ;; container.  Normally, this does nothing, but
    ;; if you want to prevent the object from being moved,
    ;; you can throw an exception here.
    (define (prepare-to-move! container thing)
      (void)))

  ;; initialize-thing!: thing -> void
  ;; EFFECT: adds thing to its initial location
  (define (initialize-thing! thing)
    (add! (thing-location thing)
          thing))

  ;; new-thing: string container -> thing
  ;; Makes a new thing with the specified adjectives, in the specified location,
  ;; and initializes it.
  (define (new-thing adjectives location)
    (local [(define thing (make-thing (string->words adjectives)
                                      '() location #f))]
      (begin (initialize-thing! thing)
             thing)))
  
;;;
;;; DOOR
;;; A portal from one room to another
;;; To join two rooms, you need two door objects, one in each room
;;;

(define-struct (door thing)

  (;; destination: container
   ;; The place this door leads to
   destination
   ;; lock status: boolean
   ;; If keycard access control is present
   lockstatus
   )
  
  #:methods
  ;; go: door -> void
  ;; EFFECT: Moves the player to the door's location and (look)s around.
  (define (go door)
    (begin
      (if (door? door)
          (if (door-lockstatus door)
          (if (or (ormap (λ (x) (= 2 (keycard-access-level x))) (filter keycard? (my-inventory)))
                   (ormap (λ (x) (not (eq? (memq (door-destination door) (keycard-privilege x)) #f))) (filter (λ (x) (= 1 (keycard-access-level x))) (filter keycard? (my-inventory)))))
              (if (and (not (person-disguised? me)) (ormap (λ (x) (securitycam-status x)) (filter securitycam? (container-accessible-contents (door-destination door)))))
                  (begin (display-line "You got caught by security camera behind the door and you have been neutralized.") (gracefuldeath))
                  (if (room-trap (door-destination door))
                      (begin (display-line "You got caught by security guards inside the room and you have been neutralized.") (gracefuldeath))
                      (begin (move! me (door-destination door))
                             (update-stats 2 2)
                             (look))))
              (display-line "This door seems to be locked."))
          (if (room-trap (door-destination door))
                      (begin (display-line "You got caught by security guards inside the room and you have been neutralized.") (gracefuldeath))
                      (begin (move! me (door-destination door))
                             (update-stats 2 2)
                             (look)))
       )
          (display-line "You are not sure what to do.")
          )
      )
    )
  )


;; join: room string room string
;; EFFECT: makes a pair of doors with the specified adjectives
;; connecting the specified rooms.
(define (join! room1 adjectives1 room2 adjectives2 locked?)
  (local [(define r1->r2 (make-door (string->words adjectives1)
                                    '() room1 #false room2 locked?))
          (define r2->r1 (make-door (string->words adjectives2)
                                    '() room2 #false room1 locked?))]

    (begin (initialize-thing! r1->r2)
           (initialize-thing! r2->r1)
           (void))))

;;;
;;; PERSON
;;; A character in the game.  The player character is a person.
;;;

(define-struct (person thing)
  (;; hunger: person's hunger level.
   ;; An integer from 0 to 20.
   hunger
   ;; thirst: person's thirst level.
   ;; An integer from 0 to 20.
   thirst
   ;; hp: player's health points.
   ;; An integer from 0 to 20.
   hp
   ;; disguised?: A boolean value.
   ;; True while the player wears a disguise prop.
   disguised?))

;; initialize-person: person -> void
;; EFFECT: do whatever initializations are necessary for persons.
(define (initialize-person! p)
  (initialize-thing! p))

;; new-person: string container -> person
;; Makes a new person object and initializes it.
(define (new-person adjectives location)
  (local [(define person
            (make-person (string->words adjectives)
                         '()
                         location
                         false
                         20
                         20
                         20
                         false))]
    (begin (initialize-person! person)
           person)))

;; This is the global variable that holds the person object representing
;; the player.  This gets reset by (start-game)
(define me empty)

;;;
;;; PROP
;;; A thing in the game that doesn't serve any purpose other than to be there.
;;;

(define-struct (prop thing)
  (;; noun-to-print: string
   ;; The user can set the noun to print in the description so it doesn't just say "prop"
   noun-to-print
   ;; examine-text: string
   ;; Text to print if the player examines this object
   examine-text
   )
  
  #:methods
  (define (noun prop)
    (prop-noun-to-print prop))

  (define (examine prop)
    (display-line (prop-examine-text prop))))

;; new-prop: string container -> prop
;; Makes a new prop with the specified description.
(define (new-prop description examine-text location bool)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define prop (make-prop adjectives '() location bool noun examine-text))]
    (begin (initialize-thing! prop)
           prop)))

;;;
;;; ADD YOUR TYPES HERE!
;;;

;;;
;;; KEYCARD
;;; A thing in game for access control of the doors.
;;;
(define-struct (keycard thing)
  (;; owner: string
   ;; The original owner of this keycard
   owner
   ;; access-level: integer number 0, 1, 2, 3
   ;; 0 = invalied (no access)
   ;; 1 = partial access (limited access with certain privilege)
   ;; 2 = admin (full access)
   access-level
   ;; privilege: door
   ;; allows certain doors to be used by keycard with access level 1
   privilege)

   #:methods
   ;; hide the noun.
   (define (noun keycard) "")
   ;; change examine to return keycard description.
   (define (examine keycard)
    (display-line (string-append
     "A purple access control card with a picture and the name: "
     (keycard-owner keycard))))
)

;; new-keycard: string, number, list of container, container -> keycard
;; Makes a new keycard with the specified parameters.
(define (new-keycard owner access-level privilege location)
  (local [(define adjs (string->words (string-append "access control card with the name " owner)))
          (define keycard (make-keycard adjs '() location #true owner access-level privilege))]
    (begin (initialize-thing! keycard)
           keycard)))

;;;
;;; SECURITYCAM
;;; A thing in game for checking if the user is in a room. Can trigger a losing scenario.
;;;
(define-struct (securitycam thing)
  ;; status: boolean
  ;; The on and off status of the cam
  (status)

   #:methods
  ;; hide the noun.
  (define (noun securitycam)
    "")
  ;; change examine to return keycard description.
  (define (examine securitycam)
    (if (securitycam-status securitycam)
    (display-line "A black security camera that is blinking red light.")
    (display-line "A black security camera. It seems to be off.")))
)

;; new-securitycam: string, container, status -> securitycam
;; Makes a new keycard with the specified parameters.
(define (new-securitycam adjectives location status)
  (local [(define adjs (string->words (string-append adjectives " security camera")))
          (define securitycam (make-securitycam adjs '() location #false status))]
    (begin (initialize-thing! securitycam)
           securitycam)))

;;;
;;; FOOD
;;; You know, you eat this.
;;;
(define-struct (food prop)
  (satiety)
  #:methods
  (define (eat food)
    (if (not (empty? (container-contents food)))
        (begin
          (display-line (string-append "¿¿ Wow! there's " (description (first (container-contents food))) " in here! ??"))
          (display-line (string-append "You found " (description (first (container-contents food))) " inside " (description food) " and it's now in your inventory."))
          (move! (first (container-contents food)) me)
          (display-line "Unfortunately you can't really eat it. Your hunger level stays the same."))
        (if (= (person-hunger me) 20)
            (begin (display-line "You are too full to eat this.")
                   (check-hunger))
            (begin
              (destroy! food)
              (display-line "Tasty!")
              (if (> (+ (person-hunger me) (food-satiety food)) 20)
                  (begin (set-person-hunger! me 20)
                         (display-line "You're full!"))
                  (set-person-hunger! me (+
                                          (person-hunger me)
                                          (food-satiety food))))
              (health-regen)
              (mystatus))))))

;; new-food string, container, string, integer -> food
;; Makes a new food object with the specified parameters.
(define (new-food description location examine-text satiety)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define food (make-food adjectives '() location true noun examine-text satiety))]
    (if (or (< satiety 0) (> satiety 20))
              (error "Bad satiety value.")
              (begin (initialize-thing! food)
           food))))

;;;
;;; BEVERAGE
;;; The thing you drink in game.
;;;
(define-struct (beverage prop)
  (satiety)
  #:methods
  (define (drink o)
        (if (= (person-thirst me) 20)
            (begin (display-line "You are too full to drink this.")
                   (check-thirst))
            (begin
              (destroy! o)
              (display-line "mmm, thirst-quenching!")
              (if (> (+ (person-thirst me) (beverage-satiety o)) 20)
                  (begin (set-person-thirst! me 20)
                         (display-line "You're fully hydrated."))
                  (set-person-thirst! me (+ (person-thirst me)
                                            (beverage-satiety o))))
              (health-regen)
              (mystatus)
              )
            )
    )
  )

;; new-beverage string, container, string, integer -> beverage
;; Makes a new beverage object with the specified parameters.
(define (new-beverage description location examine-text satiety)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define beverage (make-beverage adjectives '() location true noun examine-text satiety))]
    (if (or (< satiety 0) (> satiety 20))
              (error "Bad satiety value.")
              (begin (initialize-thing! beverage)
                     beverage)
    )
  )
)

;;;
;;; STORAGE
;;; Storage container for storing stuff.
;;;
(define-struct (storage prop)
  ()

  #:methods
  (define (open storage)
    (begin
      (update-stats 1 1)
      (describe-contents storage)
      (move! me storage)
      (newline)
      (printf "Close the ~A with (close).~%"
                 (noun storage))
      )))

;; new-storage string, container, boolean, string -> storage
;; Makes a new storage object with the specified parameters.
(define (new-storage description location bool examine-text)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define storage (make-storage adjectives '() location bool noun examine-text))]
    (begin (initialize-thing! storage)
           storage)))

;;;
;;; DISGUISE
;;; Type of thing the player can wear to disguise from camera.
;;;
(define-struct (disguise prop)
  ()

  #:methods
  (define (wear x)
    (unless (person-disguised? me)
    ;; Check if the item is in player's inv
    (if (equal? (thing-location x) me)
        (begin
          (update-stats 4 1)
          (set-person-disguised?! me true)
          (display-line "You are now disguised.")
          (display-line "You realized that being disguised requires more physical effort.")
          (mystatus))
        ;; If not, make sure the player takes it
        (begin
          (move! x me)
          (display-line "You took the item.")
          (update-stats 5 2)
          (set-person-disguised?! me true)
          (display-line "You are now disguised.")
          (display-line "You realized that being disguised requires more physical effort.")
          (mystatus))))))

;; new-disguise string, container, string -> disguise
;; Makes a new disguise object with the specified parameters.
  (define (new-disguise description location examine-text)
    (local [(define words (string->words description))
            (define noun (last words))
            (define adjectives (drop-right words 1))
            (define disguise (make-disguise adjectives '() location true noun examine-text))]
      (begin (initialize-thing! disguise)
             disguise)))

;;;
;;; HOLYGRAIL
;;; Type of thing the player takes to win the game.
;;;
(define-struct (holygrail prop)
  ()

  #:methods
  (define (take x)
    (local [(define cuba (new-room "cuba"))]
    (begin (move! me cuba)
           (newline)
           (display-line"Congrats!")
           (sleep 1)
           (display "It took you ")
           (display counter)
           (display " steps to finish the game.")
           (newline)
           (sleep 5)
           (display-line "You took the admin keycard and unlocked the way out.")
           (sleep 2)
           (display-line "After escaping from prison, you moved to Cuba and became a carpet salesman.")
           (sleep 3)
           (display-line "You got cancer at age 50, and you turned out victorious at the end.")
           (sleep 4)
           (display-line "You had a happy life and passed away in bed at the age of 93.")
           (sleep 5)
           (display-line "But if you didn't escape, you'll be the founder of next big tech firm.")
           (sleep 6)
           (display-line "Oof. At least you beat the game! Congratulations and Enjoy your break.")
           (sleep 10)
           (endcredit)))))

;; new-holygrail string, container, string -> holygrail
;; Makes a new disguise object with the specified parameters.
  (define (new-holygrail description location examine-text)
    (local [(define words (string->words description))
            (define noun (last words))
            (define adjectives (drop-right words 1))
            (define holygrail (make-holygrail adjectives '() location true noun examine-text))]
      (begin (initialize-thing! holygrail)
             holygrail)))
          

;;;
;;; LAPTOP
;;; Laptop: allows hacking.
;;;
(define-struct (laptop thing)
  ;; batterylevel: integer number from 0 to 20
  ;; How much battery is left.
  (batterylevel)

   #:methods
  ;; charge: laptop -> void
  ;; Charge the laptop if a charger is present.
  (define (charge laptop)
    (if (nearby? laptopcharger?)
      (if (have-a-in-room? poweroutlet?)
          (begin (update-stats 4 2)
                 (if (> (+ (laptop-batterylevel laptop) 5) 20)
                     (set-laptop-batterylevel! laptop 20)
                     (set-laptop-batterylevel! laptop (+ (laptop-batterylevel laptop) 5)))
                 (check-battery)
                 (mystatus)
                 )
          (display-line "There's no power outlet nearby."))
      (display-line "You don't know how to charge your laptop without a charger."))
    )
  ;; hide the noun.
  (define (noun laptop)
    "")
  ;; change examine to return keycard description.
  (define (examine laptop)
    (if (= 0 (laptop-batterylevel laptop))
    (display-line "A Linix laptop with a Northwestern sticker on the front. The battery seems dead.")
    (if (= 20 (laptop-batterylevel laptop))
    (display-line "A Linix laptop with a Northwestern sticker on the front. The battery seems fully charged.")
    (display-line "A Linix laptop with a Northwestern sticker on the front. There are some battery left.")))
    )
)

;; new-laptop: string, container, number -> laptop
;; Makes a laptop with the specified parameters.
(define (new-laptop adjectives location batterylevel)
  (local [(define adjs (string->words (string-append adjectives " laptop")))
          (define laptop (make-laptop adjs '() location #true batterylevel))]
    (begin (initialize-thing! laptop)
           laptop)))


;;;
;;; LAPTOPCHARGER
;;; Laptop Charger: allows charging.
;;;
(define-struct (laptopcharger thing)
  ()

   #:methods
  ;; hide the noun.
  (define (noun laptopcharger)
    "")
  ;; change examine to return keycard description.
  (define (examine laptopcharger)
    (display-line "A laptop charger for Banana Pro laptop. Needs an outlet: AC 110V 10A.")
    )
)

;; new-laptopcharger: string, container, number -> laptopcharger
;; Makes a laptop with the specified parameters.
(define (new-laptopcharger adjectives location)
  (local [(define adjs (string->words (string-append adjectives " laptop charger")))
          (define laptopcharger (make-laptopcharger adjs '() location #true))]
    (begin (initialize-thing! laptopcharger)
           laptopcharger)))

;;;
;;; POWEROUTLET
;;; Power Outlet: allows charging.
;;;
(define-struct (poweroutlet thing)
  ()

   #:methods
  ;; hide the noun.
  (define (noun laptopcharger)
    "")
  ;; change examine to return keycard description.
  (define (examine laptop)
    (display-line "A standard two ports power outlet. AC 110V 10A.")
    )
)

;; new-poweroutlet: string, container, number -> poweroutlet
;; Makes a power outlet with the specified parameters.
(define (new-poweroutlet adjectives location)
  (local [(define adjs (string->words (string-append adjectives " power outlet")))
          (define poweroutlet (make-poweroutlet adjs '() location #false))]
    (begin (initialize-thing! poweroutlet)
           poweroutlet)))

;;;
;;; USER COMMANDS
;;;

(define (look)
  (begin (if (storage? (here))
             (printf "You are looking at a ~A.~%"
                 (description (here)))
             (printf "You are in ~A.~%"
                 (description (here))))
         (describe-contents (here))
         (mystatus)
         (void)
  )
)

(define-user-command (look) "Prints what you can see in the room")

(define (inventory)
  (if (empty? (my-inventory))
      (printf "You don't have anything.~%")
      (begin (printf "You have:~%")
             (for-each print-description (my-inventory)))))

(define-user-command (inventory)
  "Prints the things you are carrying with you")

(define-user-command (examine thing)
  "Takes a closer look at the thing")

(define-user-command (take thing)
  "Moves thing to your inventory")

(define (drop thing)
  (move! thing (here)))

(define-user-command (drop thing)
  "Removes thing from your inventory and places it in the room")

(define (put thing container)
  (move! thing container))

(define-user-command (put thing container)
  "Moves the thing from its current location and puts it in the container")

(define (help)
  (for-each (λ (command-info)
              (begin (display (first command-info))
                     (newline)
                     (display (second command-info))
                     (newline)
                     (newline)))
            (all-user-commands)))

(define-user-command (help)
  "Displays this help information")

(define-user-command (go door)
  "Go through the door to its destination")

(define (check condition)
  (if condition
      (display-line "Check succeeded")
      (error "Check failed!!!")))

(define-user-command (check condition)
  "Throws an exception if condition is false.")

;;;
;;; ADD YOUR COMMANDS HERE!
;;;

;; mystatus: displays player's hunger, thirst, and hp.
(define (mystatus)
  (begin
    (newline)
    (display-line "Player Status: ")
    (check-hunger)
    (check-thirst)
    (check-health)
    (unless (not (person-disguised? me))
      (display-line "~sneaky sneaky~"))
    )
  )

(define-user-command (mystatus)
  "Displays the player's hunger, thirst, and health.")

;; check-hunger: displays player's hunger level
(define (check-hunger)
  (begin
    (display "Hunger: ")
    (display (person-hunger me))
    (display "/20 | ")
    (bargraph (person-hunger me))
    (unless (> (person-hunger me) 0)
      (display-line "⚠ Your stomach is empty and you are dying because of hunger."))
  )
)

(define-user-command (check-hunger)
  "Displays the player's hunger level")

;; check-thirst: displays player's thirst level
(define (check-thirst)
  (begin
    (display "Thirst: ")
    (display (person-thirst me))
    (display "/20 | ")
    (bargraph (person-thirst me))
    (unless (> (person-thirst me) 0)
      (display-line "⚠ You are dying due to lack of hydration."))
  )
)

(define-user-command (check-thirst)
  "Displays the player's thirst level")

;; check-thirst: displays player's health level
(define (check-health)
  (begin
    (display "Health: ")
    (display (person-hp me))
    (display "/20 | ")
    (bargraph (person-hp me))
  )
)

(define-user-command (check-battery)
  "Displays the laptop's battery level")

;; check-battery: displays the laptop's battery level
(define (check-battery)
  (begin
    (display "Battery: ")
    (display (laptop-batterylevel (the laptop)))
    (display "/20 | ")
    (bargraph (laptop-batterylevel (the laptop)))
    (unless (< (laptop-batterylevel (the laptop)) 10)
      (display-line "Battery sufficiently charged."))
  )
)

(define-user-command (check-health)
  "Displays the player's health level")

;; close: close the storage
(define (close)
  (if (storage? (thing-location me))
      (begin
        (display-line (string-append "You closed the " (description (thing-location me))))
        (move! me (thing-location (thing-location me)))
        (look))
      (display-line "You haven't opened a storage object.")))

(define-user-command (open storage)
  "Opens a storage object, giving the player access to its contents")

(define-user-command (close)
  "Closes a storage object the player opened.")

(define-user-command (eat food)
  "Consumes a food object, lowering the player's hunger level")

(define-user-command (drink beverage)
  "Drinks a beverage object, lowering the player's thirst level")

(define-user-command (wear disguise)
  "Takes a disguise object if the player hasn't already, and disguises the player")

;; Get rid of the disguise. Updates disguised? status.
(define (remove-disguise)
  (if (person-disguised? me)
      (begin
        (update-stats 0 0)
        (set-person-disguised?! me false)
        (display-line "You are no longer disguised."))
      (display-line "You aren't wearing a disguise.")))

(define-user-command (remove-disguise)
  "Un-disguises the player, but leaves the object in their inventory")

(define-user-command (hack thing)
  "Try to hack the thing")

(define-user-command (charge thing)
  "Use the charger to charge the thing.")

;; hack: securitycam -> void
;; Change the status of the camera.
(define (hack securitycam)
  (if (nearby? laptop?)
      (if (ormap (λ (x) (> (laptop-batterylevel x) 9)) (filter laptop? (my-inventory)))
      (if (securitycam? securitycam)
          (begin (update-stats 1 1)
                 (set-securitycam-status! securitycam #f)
                 (display-line "#     #    #     #####  #    # ####### ######  ### ")
                 (display-line "#     #   # #   #     # #   #  #       #     # ### ")
                 (display-line "#     #  #   #  #       #  #   #       #     # ### ")
                 (display-line "####### #     # #       ###    #####   #     #  #  ")
                 (display-line "#     # ####### #       #  #   #       #     #     ")
                 (display-line "#     # #     # #     # #   #  #       #     # ### ")
                 (display-line "#     # #     #  #####  #    # ####### ######  ### ")
                 (set-laptop-batterylevel! (the laptop) (- (laptop-batterylevel (the laptop)) 10))
                 (mystatus)
                 )
          (display-line "You are not sure how to hack that."))
      (display-line "You tried but your laptop doesn't have enough battery."))
      (display-line "You don't have a a device that you can use to hack things with."))
  )

;;;
;;; THE GAME WORLD - FILL ME IN
;;;

;; start-game: -> void
;; Recreate the player object and all the rooms and things.
(define (start-game)
  ;; Fill this in with the rooms you want
  (local [(define jail-cell (new-room "solitary confinement"))
          (define jail-cell2 (new-room "holding cell 1"))
          (define jail-cell3 (new-room "holding cell 2"))
          (define jail-cell4 (new-room "holding cell 3"))
          (define jail-cell5 (new-room "holding cell 4"))
          (define jail-cell6 (new-room "holding cell 5"))
          (define jail-cell7 (new-room "holding cell 6"))
          (define jail-cell8 (new-room "holding cell 7"))
          (define jail-cell9 (new-room "holding cell 8"))
          (define cell-hallway (new-room "cell area hallway"))
          (define common-lobby (new-room "common area lobby"))
          (define cafeteria (new-room "cafeteria"))
          (define kitchen (new-room "kitchen"))
          (define visitation (new-room "visitation"))
          (define courtyard (new-room "courtyard"))
          (define stowage (new-room "stowage"))
          (define common-restroom (new-room "cleaning cupboard"))
          (define cleaning-cupboard (new-room "cleaning cupboard"))
          (define admin-hallway (new-room "administrative hallway"))
          (define admin-restroom (new-room "administrative restroom"))
          (define parole-room (new-room "parole room"))
          (define security (new-room "security"))
          (define warden-office (new-room "warden office"))
          (define fridge (new-storage "large fridge" kitchen false "This thing is huge!"))
          (define pantry (new-storage "pantry" kitchen false "What’s in here?"))
          (define desk (new-storage "desk with drawers" stowage false "There’s a laptop on the desk. I wonder if there’s anything useful inside the desk?"))
          (define closet (new-storage "closet" stowage false "I wonder what’s in here? Could be worth checking out."))
          (define cake (new-food "birthday cake" warden-office "The cake is a lie" 20))
          (define warden-desk (new-storage "warden desk" warden-office #f "Let’s see what the warden has in here."))
          (define secretary-desk (new-storage "secretary desk" warden-office #f "Cool antique desk! It has a lot of compartments."))]

    (begin (czero)
           (set! me (new-person "Willie the Wildcat" jail-cell)) 
           (set-room-trap! security #true)
;; Add join commands to connect your rooms with doors
           (join! jail-cell "hallway"
                     cell-hallway "solitary confinement" #f)
           (join! jail-cell2 "hallway"
                   cell-hallway "1st holding cell" #t)
           (join! jail-cell3 "hallway"
                   cell-hallway "2nd holding cell" #t)
           (join! jail-cell4 "hallway"
                   cell-hallway "3rd holding cell" #t)
           (join! jail-cell5 "hallway"
                   cell-hallway "4th holding cell" #t)
           (join! jail-cell6 "hallway"
                   cell-hallway "5th holding cell" #t)
           (join! jail-cell7  "hallway"
                   cell-hallway "6th holding cell" #t)
           (join! jail-cell8 "hallway"
                   cell-hallway "7th holding cell" #t)
           (join! jail-cell9 "hallway"
                   cell-hallway "8th holding cell" #t)

           (join! cell-hallway "lobby"
            common-lobby "cell hallway" #f)
           (join! common-lobby "cafeteria"
                   cafeteria "lobby" #f)
           (join! cafeteria "kitchen"
                    kitchen "cafeteria" #f)
           (join! common-lobby "visitation"
                  visitation "lobby" #t)
           (join! common-lobby "courtyard"
                  courtyard "lobby" #f)

           (join! common-lobby "stowage"
                 stowage "lobby" #t)
           (join! common-lobby "restroom"
                  common-restroom "lobby" #f)
           (join! common-lobby "cleaning cupboard"
                  cleaning-cupboard "lobby" #f)

           (join! common-lobby "admin hallway"
                  admin-hallway "lobby" #t)
           (join! admin-hallway "admin restroom"
                  admin-restroom "admin hallway" #f)
           (join! admin-hallway "parole office"
                  parole-room "admin hallway" #t)
           (join! admin-hallway "security office"
                  security "admin hallway" #t)
           (join! admin-hallway "warden office"
                  warden-office "admin-hallway" #t)
                  
           ;; Add code here to add things to your rooms
       
           ;; Install the cameras
           (new-securitycam "black" cell-hallway #f)
           (new-securitycam "black" common-lobby #f)
           (new-securitycam "black" cafeteria #f)
           (new-securitycam "black" visitation #f)
           (new-securitycam "black" courtyard #f)
           (new-securitycam "black" admin-hallway #t)
           (new-securitycam "black" parole-room #t)
           (new-securitycam "black" warden-office #t)
           (new-securitycam "black" security #t)

           ;; Install the power outlets
           (new-poweroutlet "white" kitchen)
           (new-poweroutlet "white" visitation)
           (new-poweroutlet "white" cleaning-cupboard)
           (new-poweroutlet "white" admin-hallway)
           (new-poweroutlet "white" admin-restroom)
           (new-poweroutlet "white" parole-room)
           (new-poweroutlet "white" security)
           (new-poweroutlet "white" warden-office)

           (new-prop "Uncomfortable bed" "Might as well sleep on the floor." jail-cell #f)
           (new-prop "dirty mirror" "A good cleaning wouldn’t hurt." jail-cell #f)

           (new-food "chocolate chip cookie" jail-cell "chocolate chip cookie" 8)
           (new-food "Cheez-its" jail-cell "These Cheez-its are looking pretty good" 8)
           (new-beverage "water bottle" jail-cell "A plastic water bottle full of water" 8)
           
           
           (new-food "Apple" jail-cell "apple" 5)
           (new-food "pie" jail-cell "pie" 20)
           
           (new-beverage "milk carton" fridge "Haven’t had one of these in a while!" 10)
           (new-food "granny smith apple" fridge "This is very sour." 6)
           (new-food "cheese stick" fridge "Yum, pepper jack." 4)
           (new-food "crackers" fridge "These are a good, basic snack." 6)
           (new-food "cereal" fridge "This would be better with a bowl and milk, but whatever." 5)
           (new-food "peanut butter" pantry "Never gets old..." 6)
           (new-food "uncooked pasta" pantry "This doesn’t look good..." 1)
           (new-food "sprinkles" pantry "Seriously?" 2)

           (new-prop "Long, rectangular tables" "There isn’t much to see here." cafeteria #f)
           (new-prop "Long, rectangular benches" "There isn’t much to see here." cafeteria #f)
           (new-food "box of chicken tenders" cafeteria "Yum!" 10)
           (new-food "banana" cafeteria "Perfectly ripe." 6)
           (new-food "Bag of potato chips" cafeteria "Original flavor" 6)
           (new-food "cup of yogurt" cafeteria "Strawberry flavor" 6)
           (new-beverage "water bottle" cafeteria "Looks pretty good." 10)
           (new-beverage "orange juice carton" cafeteria "It isn't breakfast!" 10)
           (new-beverage "apple juice" cafeteria "It's always good" 10)
           (new-beverage "chocolate milk carton" cafeteria "10/10 drink" 10)
           (new-prop "big dude eating food" "He surely looks like need a lot of food." cafeteria #f)

           (new-prop "three visitation sections: each made up of two wooden chairs, two corded telephones, and a window in the middle" "These look like pretty generic visitation tables." visitation #f)  
           (new-disguise "guard’s uniform" closet "What a convenient thing to find!")

           (new-prop "guy lifting weights" "He sure looks like he's having fun!" courtyard #f)
           (new-prop "broken tetherball pole" ":(" courtyard #f)
           (new-prop "prisoner selling contraband" "What’s he selling?" courtyard #f)
           (new-prop "guy running aroound" "He's getting fit. Good for him." courtyard #f)

           (new-prop "mop" "This doesn’t seem incredibly useful." cleaning-cupboard #t) 
           (new-prop "mop bucket" "This doesn’t seem incredibly useful." cleaning-cupboard #t)
           (new-prop "broom" "Neat." cleaning-cupboard #t)
    
           (new-prop "sink" "It is a sink." common-restroom #f)
           (new-prop "mirror" "It’s cleaner than the one in the cell." common-restroom #f)
           (new-prop "toilet stall" "It’s a toilet stall." common-restroom #f)
           (new-prop "guy taking a loo" "Smelly!" common-restroom #f)

           (new-prop "nondescript brown boxes" "These look boring." stowage #f)
           (new-prop "laundry basket" "Cool?" stowage #f)
            
           (new-prop "wall of sinks" "They are two sinks." admin-restroom #f)
           (new-prop "wall of mirrors" "They are two mirrors." admin-restroom #f)
           (new-prop "toilet stall" "They are a toilet stall." admin-restroom #f)

           (new-beverage "catorade" parole-room "A famous sports drink." 20)
           (new-beverage "nerds" parole-room "The candy, nerds." 5)
           
           (new-prop "Donald's tax returns" "I guess the cake really was a lie..." cake #t) 
           (new-prop "table with a chocolate fungecake on it" "Nice table." warden-office #f)
           (new-prop "ballpoint pen" "This is oddly fancy for a pen." warden-desk #t)
           (new-prop "generic documents" "This isn’t really a surprise." warden-desk #t)
           (new-food "pack of gum" warden-desk "Spearmint, good choice." 2)
           (new-beverage "boxed water" warden-desk "Boxed water is better." 10)
           (new-prop "computer" "How did he get a 3080 so fast?" warden-office #f)
           (new-prop "computer monitor" "240Hz. Looks pretty nice." warden-office #f)
           (new-holygrail "Admin Keycard" secretary-desk "A access control card with a name Mortimer S. Schapiro.")

           
           ;; Keycards In the Game
           (new-keycard "Janitor Rick Sanchez" 1 (list visitation stowage admin-hallway parole-room common-lobby) cleaning-cupboard)
           (new-keycard "US Attorney Rudy Giuliani" 2 '() parole-room)
           (new-keycard "Inmate Willie the Wildcat" 0 '() me)

           ;; Laptop related
           (new-laptop "silver Banana Pro" stowage 1)
           (new-laptopcharger "black" desk)

           (check-containers!)
           (void))))



;;;
;;; PUT YOUR WALKTHROUGHS HERE
;;;
(define-walkthrough win
  (take (the pie))
  (take (the water))
  (take (the chocolate chip cookie))
  (take (the Cheez-its))
  (take (the Apple))
  (go (the hallway))
  (go (the lobby))
  (go (the cleaning cupboard))
  (take (the access control card with the name Janitor Rick Sanchez))
  (go (the door))
  (go (the stowage))
  (take (the laptop))
  (open (the closet))
  (take (the uniform))
  (close)
  (drink (the water))
  (eat (the pie))
  (open (the desk))
  (take (the charger))
  (close)
  (go (the door))
  (go (the cafeteria))
  (drink (the milk))
  (take (the apple juice))
  (take (the orange juice))
  (take (the potato chips))
  (take (the chicken tenders))
  (go (the kitchen))
  (check-battery)
  (charge (the laptop))
  (charge (the laptop))
  (charge (the laptop))
  (charge (the laptop))
  (drink (the apple juice))
  (eat (the Apple))
  (open (the fridge))
  (take (the cereal))
  (take (the crackers))
  (take (the cheese stick))
  (take (the granny smith apple))
  (take (the milk carton))
  (close)
  (go (the door))
  (eat (the cup of yogurt))
  (drink (the water bottle))
  (take (the banana))
  (go (the lobby))
  (eat (the crackers))
  (eat (the cereal))
  (drink (the milk))
  (inventory)
  (eat (the cheese))
  (eat (the cookie))
  (drink (the orange juice))
  (look)
  (wear (the uniform))
  (go (the admin hallway))
  (hack (the camera))
  (go (the parole office))
  (hack (the camera))
  (remove-disguise)
  (take (the access control card with the name US Attorney Rudy Giuliani))
  (drink (the catorade))
  (eat (the nerds))
  (go (the door))
  (eat (the chicken tenders))
  (eat (the apple))
  (charge (the laptop))
  (charge (the laptop))
  (wear (the uniform))
  (go (the warden office))
  (hack (the camera))
  (eat (the banana))
  (eat (the potato chips))
  (open (the warden desk))
  (drink (the boxed water))
  (close)
  (open (the secretary desk))
  (take (the Admin Keycard))
  )

;;;
;;; UTILITIES
;;;

;; gracefuldeath:
;; Makes the death graceful.
(define (gracefuldeath)
  (local [(define heaven (new-room "heaven"))]
  (begin (move! me heaven)
         (display-line "Do you want to try again? Input (Y) for yes, (N) for no:"))))
(define (Y) (begin (start-game) (look)))
(define (N) (endcredit))

;; endcredit:
;; Displays an awesome end credit.
(define (endcredit)
  (begin (newline)
         (display-line "Adventure Game for CS111 - Fall 2020")
         (sleep 1)
         (display-line "Made By Sean Carlson and Charles Zhou")
         (sleep 5)
         (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWWWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMWX00KWMMMMMMMWKKKKKKKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMN0xolOWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMWOc,;oKWMMMMMNOdc;cdONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWWMMMMMN0l',kWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMWKc.':xNMMMMMMWk;dWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXddXMMMMMWO,,kWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMW0okWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMXl;:,'lOWMMMMWk:xWMMMWNKOOOOKNWMMMMNX0KWWKOOXWNXx,:k00O0WWO;,kWX0OkOKWMMNK000KNWX00000XWWX000KNMMNKOkO0XWMMMMNKOOOO0XWWXKl'lO00OKMMWNKOkO0XWMMMWX00NWKOOXWWNK0XWX0Ok0KWMMMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMXlc0Oc',oKWMMWk;xWMWKdldkkkdccdKWWKd;,ckdc;:ONOd;',lolo0WWO;,okxdl:,;oKWXd;,ckNWKl,;okKWWKd:lOXXxodxkxlcdXMW0lokOkl;dNXkl,';loldXMXxldkkxocdXWXd:,cxdc;;kXOc,,okxdl:,;dXMMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMXlcKWXx;':xNMWk;xWW0:;xNMMMWKl':OWW0c';x0XKKNMWXo'cKWWWWMMO;,dXWWWXx;'dNMXo':0MMWk,'o0XMMNxoKWKl;xNWWW0:'xNNo,oKWMXxOWMWKc'dNWWWWKl;xNWWW0:'dNWKl';d0XXKXWNd,,dXWWWXd,,kWMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMXlcKMMWKo,'cONk;xWXl'cKMMMMMM0:.lXMNd'lXMMMMMMMNd'lXMMMMMMO;,kWMMMMXl'oNMM0:'oXMXd:,,ckWWOo0WNd';dOOkkdccxNWk;';lx0XWMMMKc'dNMMMWx';dOOOkdc:xNMWx':KMMMMMMM0;,kWMMMMK:'xWMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMXlcKMMMMNOc',oo;xWKc.cKMMMMMMKc.lXMNd'lXMMMMMMMNo'lXMMMMMMO;,kWMMMMXl'oNMMWx,;kXxd0o'.:00okWMNo':ONNNNNNNWMMWKxl;,,ckNMMKc'dWMMMNo';ONNNNNNNWMMWx'cKMMMMMMM0;,kWMMMMK:'xWMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMXlcKMMMMMWXx;'',xWNd';kWMMMMWO;,xWMNd'lXMMMMMMMNo'lXMMMMMMO;,kWMMMMXl'oNMMMXo'codKWKc.'cldXMMWk,'oXWMMMMMMMN0XWWXOl';OMMK:'dWMMMWk,'oXMMMMMMMMMWx'cKMMMMMMM0;,kWMMMMK:'xWMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMWN0c:kXWMMMMMWKo,'xWMXd;;xKNNXOcckNMWXo':ONWMMMMMWx,;d0KKXWNk,'dXWMMW0c.c0NWMW0:'c0WMWOl;'cKMMMMNx;':dkOOOOKWKccONWNx;c0MMXl':kKKKNNx;':dO0OOkKWWXo':ONWMMMMNk;'dXWMMWO;'oKWMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMXkdoldkKWMMMMMMNOoOWMMWKxoodxxxOXWMWXkoloxONMMMMMMXxccoxOXN0dolokKWWKxolox0NMMNko0WMMMNXkoOWMMMMMWKxocclox0NMNOooxkxdxKWMMWKoccox0NMWKxoccloxONWXOdlodOXMMMN0xolokKWW0xoloxKWMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMWWWWWWMMMMMMMMMMWWMMMMMMWWNNWMMMMMMMWWWWWWMMMMMMMMMWWWWMMMWWWWWWMMMMWWWWWWMMMMWWMMMMMMMMWMMMMMMMMMMWWWWWMMMMMMWWNNWWMMMMMMMWWWWMMMMMMMWWWWWMMMMMWWWWWWMMMMMWWWWWWMMMMWWWWWMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
         (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
         (newline)
         (display-line "-- THE END --")))
  

;; loadingscreen:
;; Displays a good looking loading screen. Because why not?
(define (loadingscreen)
  ;; Intro
  (begin   (display-line "- THIS GAME IS BEST PLAYED UNDER FULL SCREEN -")
           (display-line "- PLEASE TRY TO ADJUST YOUR RACKET WIDTH NOW -")
           (sleep 10)
           (newline)
           (display-line "You are a computer science genius studying at Northwestern.")
           (sleep 3)
           (display-line "You hacked into the U.S. Bank ATM system for some quick cash because MOD Pizza is too expensive and you couldn’t afford it.")
           (sleep 3)
           (display-line "Unfortunately, you got caught by Ian. He had to turn you in; now you're in jail.")
           (sleep 3)
           (display-line "Your mission, should you choose to accept it, is to break out of jail.")
           (sleep 3)
           (newline)
           (display-line "█ █▄░█ █ ▀█▀ █ ▄▀█ █░░ █ ▀█ █ █▄░█ █▀▀   █▀▀ ▄▀█ █▀▄▀█ █▀▀   █▀▀ █▄░█ █▀▀ █ █▄░█ █▀▀")
           (display-line "█ █░▀█ █ ░█░ █ █▀█ █▄▄ █ █▄ █ █░▀█ █▄█   █▄█ █▀█ █░▀░█ ██▄   ██▄ █░▀█ █▄█ █ █░▀█ ██▄")
           (newline)
           (display-line "AMIBIOS (C) 1989-1994 American Megatrends, Inc.")
           (sleep 0.3)
           (display-line "CPU : Texas Instruments Ti486dx2-66")
           (sleep 0.3)
           (display-line "   Speed: 66 MHz Count : 1")
           (sleep 1)
           (display-line "Connected at 115200 bps...")
           (sleep 0.5)
           (display-line "Logging on to Zoom University....")
           (sleep 2)
           (display-line "Hacking Canvas Scores.....")
           (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWWWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMWX00KWMMMMMMMWKKKKKKKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMN0xolOWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMWOc,;oKWMMMMMNOdc;cdONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWWMMMMMN0l',kWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMWKc.':xNMMMMMMWk;dWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXddXMMMMMWO,,kWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMW0okWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMXl;:,'lOWMMMMWk:xWMMMWNKOOOOKNWMMMMNX0KWWKOOXWNXx,:k00O0WWO;,kWX0OkOKWMMNK000KNWX00000XWWX000KNMMNKOkO0XWMMMMNKOOOO0XWWXKl'lO00OKMMWNKOkO0XWMMMWX00NWKOOXWWNK0XWX0Ok0KWMMMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMXlc0Oc',oKWMMWk;xWMWKdldkkkdccdKWWKd;,ckdc;:ONOd;',lolo0WWO;,okxdl:,;oKWXd;,ckNWKl,;okKWWKd:lOXXxodxkxlcdXMW0lokOkl;dNXkl,';loldXMXxldkkxocdXWXd:,cxdc;;kXOc,,okxdl:,;dXMMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMXlcKWXx;':xNMWk;xWW0:;xNMMMWKl':OWW0c';x0XKKNMWXo'cKWWWWMMO;,dXWWWXx;'dNMXo':0MMWk,'o0XMMNxoKWKl;xNWWW0:'xNNo,oKWMXxOWMWKc'dNWWWWKl;xNWWW0:'dNWKl';d0XXKXWNd,,dXWWWXd,,kWMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMXlcKMMWKo,'cONk;xWXl'cKMMMMMM0:.lXMNd'lXMMMMMMMNd'lXMMMMMMO;,kWMMMMXl'oNMM0:'oXMXd:,,ckWWOo0WNd';dOOkkdccxNWk;';lx0XWMMMKc'dNMMMWx';dOOOkdc:xNMWx':KMMMMMMM0;,kWMMMMK:'xWMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMXlcKMMMMNOc',oo;xWKc.cKMMMMMMKc.lXMNd'lXMMMMMMMNo'lXMMMMMMO;,kWMMMMXl'oNMMWx,;kXxd0o'.:00okWMNo':ONNNNNNNWMMWKxl;,,ckNMMKc'dWMMMNo';ONNNNNNNWMMWx'cKMMMMMMM0;,kWMMMMK:'xWMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMXlcKMMMMMWXx;'',xWNd';kWMMMMWO;,xWMNd'lXMMMMMMMNo'lXMMMMMMO;,kWMMMMXl'oNMMMXo'codKWKc.'cldXMMWk,'oXWMMMMMMMN0XWWXOl';OMMK:'dWMMMWk,'oXMMMMMMMMMWx'cKMMMMMMM0;,kWMMMMK:'xWMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMWN0c:kXWMMMMMWKo,'xWMXd;;xKNNXOcckNMWXo':ONWMMMMMWx,;d0KKXWNk,'dXWMMW0c.c0NWMW0:'c0WMWOl;'cKMMMMNx;':dkOOOOKWKccONWNx;c0MMXl':kKKKNNx;':dO0OOkKWWXo':ONWMMMMNk;'dXWMMWO;'oKWMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMXkdoldkKWMMMMMMNOoOWMMWKxoodxxxOXWMWXkoloxONMMMMMMXxccoxOXN0dolokKWWKxolox0NMMNko0WMMMNXkoOWMMMMMWKxocclox0NMNOooxkxdxKWMMWKoccox0NMWKxoccloxONWXOdlodOXMMMN0xolokKWW0xoloxKWMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMWWWWWWMMMMMMMMMMWWMMMMMMWWNNWMMMMMMMWWWWWWMMMMMMMMMWWWWMMMWWWWWWMMMMWWWWWWMMMMWWMMMMMMMMWMMMMMMMMMMWWWWWMMMMMMWWNNWWMMMMMMMWWWWMMMMMMMWWWWWMMMMMWWWWWWMMMMMWWWWWWMMMMWWWWWMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
           (display-line "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")
           (sleep 10)
           (newline)
           (display-line "Your daily free time has just begun. Your cell door is unlocked.")
           (sleep 1)
           (display-line "Good Luck!")
           (sleep 3))
  )
  
;; bargraph: number -> void:
;; Outputs a string as a hunger/thirst bar.
(define (bargraph value)
  (if (or (< value 0) (> value 20))
      (error "Illegal bargraph value")
      (display-line (list->string (append (make-list value #\■) (make-list (- 20 value) #\□))))
      )
  )

;; health-regen:
;; Health regen if hunger and thirst levels are full.
(define (health-regen)
  (if (and (= (person-hunger me) 20) (= (person-thirst me) 20))
      (if (< (+ (person-hp me) 10) 20)
          (set-person-hp! me (+ (person-hp me) 10))
          (set-person-hp! me 20))
    (void))
  )

;; update-stats number number -> void:
;; Updates a player's hunger and thirst when an action is performed
(define (update-stats hunger thirst)
  (begin
    (c++)
    ;; Penalty for disguised.
    (if (person-disguised? me)
      (begin (if (> (- (person-hunger me) (+ hunger 1)) 0)
                 (set-person-hunger! me (- (person-hunger me) (+ hunger 1)))
                 ;; Deducting health value when hunger level is empty.
                 (if (< (+ (person-hp me) (- (person-hunger me) (+ hunger 1))) 1)
                     (begin (display-line "Oof. You died.") (gracefuldeath))
                     (begin (set-person-hp! me (+ (person-hp me) (- (person-hunger me) (+ hunger 1))))
                            (set-person-hunger! me 0))))
             (if (> (- (person-thirst me) (+ thirst 3)) 0)
                 (set-person-thirst! me (- (person-thirst me) (+ thirst 3)))
                  ;; Deducting health value when thirst level is empty.
                 (if (< (+ (person-hp me) (- (person-thirst me) (+ thirst 3))) 1)
                     (begin (display-line "Oof. You died.") (gracefuldeath))
                     (begin (set-person-hp! me (+ (person-hp me) (- (person-thirst me) (+ thirst 3))))
                            (set-person-thirst! me 0)))))
      ;; Normal procedures.
      (begin (if (> (- (person-hunger me) hunger) 0)
                 (set-person-hunger! me (- (person-hunger me) hunger))
                 ;; Deducting health value when hunger level is empty.
                 (if (< (+ (person-hp me) (- (person-hunger me) hunger)) 1)
                     (begin (display-line "Oof. You died.") (gracefuldeath))
                     (begin (set-person-hp! me (+ (person-hp me) (- (person-hunger me) hunger)))
                            (set-person-hunger! me 0))))
             (if (> (- (person-thirst me) thirst) 0)
                 (set-person-thirst! me (- (person-thirst me) thirst))
                 ;; Deducting health value when thirst level is empty.
                 (if (< (+ (person-hp me) (- (person-thirst me) thirst)) 1)
                     (begin (display-line "Oof. You died.") (gracefuldeath))
                     (begin (set-person-hp! me (+ (person-hp me) (- (person-thirst me) thirst)))
                            (set-person-thirst! me 0)))))
    )
  )
)
    
;; here: -> container
;; The current room the player is in
(define (here)
  (thing-location me))

;; stuff-here: -> (listof thing)
;; All the stuff in the room the player is in
(define (stuff-here)
  (container-accessible-contents (here)))

;; stuff-here-except-me: -> (listof thing)
;; All the stuff in the room the player is in except the player.
(define (stuff-here-except-me)
  (remove me (stuff-here)))

;; my-inventory: -> (listof thing)
;; List of things in the player's pockets.
(define (my-inventory)
  (container-accessible-contents me))

;; accessible-objects -> (listof thing)
;; All the objects that should be searched by find and the.
(define (accessible-objects)
  (append (stuff-here-except-me)
          (my-inventory)))

;; have?: thing -> boolean
;; True if the thing is in the player's pocket.
(define (have? thing)
  (eq? (thing-location thing)
       me))

;; have-a?: predicate -> boolean
;; True if the player has something satisfying predicate in their pocket.
(define (have-a? predicate)
  (ormap predicate
         (my-inventory)))

;; nearby?: predicate -> boolean
;; True if something satisfying predicate is in reach.
(define (nearby? predicate)
  (ormap predicate
         (accessible-objects)))

;; have-a-in-room?: predicate -> boolean
;; True if the player as something satisfying predicate in their pocket or the room.
(define (have-a-in-room? predicate)
  (ormap predicate
         (stuff-here-except-me)))

;; find-the: (listof string) -> object
;; Returns the object from (accessible-objects)
;; whose name contains the specified words.
(define (find-the words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (accessible-objects)))

;; find-within: container (listof string) -> object
;; Like find-the, but searches the contents of the container
;; whose name contains the specified words.
(define (find-within container words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (container-accessible-contents container)))

;; find: (object->boolean) (listof thing) -> object
;; Search list for an object matching predicate.
(define (find predicate? list)
  (local [(define matches
            (filter predicate? list))]
    (case (length matches)
      [(0) (error "There's nothing like that here")]
      [(1) (first matches)]
      [else (error "Which one?")])))

;; everything: -> (listof container)
;; Returns all the objects reachable from the player in the game
;; world.  So if you create an object that's in a room the player
;; has no door to, it won't appear in this list.
(define (everything)
  (local [(define all-containers '())
          ; Add container, and then recursively add its contents
          ; and location and/or destination, as appropriate.
          (define (walk container)
            ; Ignore the container if its already in our list
            (unless (member container all-containers)
              (begin (set! all-containers
                           (cons container all-containers))
                     ; Add its contents
                     (for-each walk (container-contents container))
                     ; If it's a door, include its destination
                     (when (door? container)
                       (walk (door-destination container)))
                     ; If  it's a thing, include its location.
                     (when (thing? container)
                       (walk (thing-location container))))))]
    ; Start the recursion with the player
    (begin (walk me)
           all-containers)))

;; print-everything: -> void
;; Prints all the objects in the game.
(define (print-everything)
  (begin (display-line "All objects in the game:")
         (for-each print-description (everything))))

;; every: (container -> boolean) -> (listof container)
;; A list of all the objects from (everything) that satisfy
;; the predicate.
(define (every predicate?)
  (filter predicate? (everything)))

;; print-every: (container -> boolean) -> void
;; Prints all the objects satisfying predicate.
(define (print-every predicate?)
  (for-each print-description (every predicate?)))

;; check-containers: -> void
;; Throw an exception if there is an thing whose location and
;; container disagree with one another.
(define (check-containers!)
  (for-each (λ (container)
              (for-each (λ (thing)
                          (unless (eq? (thing-location thing)
                                       container)
                            (error (description container)
                                   " has "
                                   (description thing)
                                   " in its contents list but "
                                   (description thing)
                                   " has a different location.")))
                        (container-contents container)))
            (everything)))

;; is-a?: object word -> boolean
;; True if word appears in the description of the object
;; or is the name of one of its types
(define (is-a? obj word)
  (let* ((str (if (symbol? word)
                  (symbol->string word)
                  word))
         (probe (name->type-predicate str)))
    (if (eq? probe #f)
        (member str (description-word-list obj))
        (probe obj))))

;; display-line: object -> void
;; EFFECT: prints object using display, and then starts a new line.
(define (display-line what)
  (begin (display what)
         (newline)
         (void)))

;; words->string: (listof string) -> string
;; Converts a list of one-word strings into a single string,
;; e.g. '("a" "red" "door") -> "a red door"
(define (words->string word-list)
  (string-append (first word-list)
                 (apply string-append
                        (map (λ (word)
                               (string-append " " word))
                             (rest word-list)))))

;; string->words: string -> (listof string)
;; Converts a string containing words to a list of the individual
;; words.  Inverse of words->string.
(define (string->words string)
  (string-split string))

;; add-a-or-an: (listof string) -> (listof string)
;; Prefixes a list of words with "a" or "an", depending
;; on whether the first word in the list begins with a
;; vowel.
(define (add-a-or-an word-list)
  (local [(define first-word (first word-list))
          (define first-char (substring first-word 0 1))
          (define starts-with-vowel? (string-contains? first-char "aeiou"))]
    (cons (if starts-with-vowel?
              "an"
              "a")
          word-list)))

;;
;; The following calls are filling in blanks in the other files.
;; This is needed because this file is in a different langauge than
;; the others.
;;
(set-find-the! find-the)
(set-find-within! find-within)
(set-restart-game! (λ () (start-game)))
(define (game-print object)
  (cond [(void? object)
         (void)]
        [(object? object)
         (print-description object)]
        [else (write object)]))

(current-print game-print)
   
;;;
;;; Start it up
;;;

(loadingscreen)
(start-game)
(look)