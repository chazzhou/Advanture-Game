;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname adventure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "adventure-define-struct.rkt")
(require "macros.rkt")
(require "utilities.rkt")
(require racket/bool)

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
    (display-line "That isn't food."))
  (define (drink o)
    (display-line "That isn't a beverage."))
  (define (open x)
    (begin
      (display-line "That object isn't a storage container")))
  (define (wear x)
    (display-line "That isn't a disguise.")))



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
  ())

;; new-room: string -> room
;; Makes a new room with the specified adjectives
 (define (new-room adjectives)
  (make-room (string->words adjectives)
             '()))

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
                                    '() location))]
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
      (update-stats)
      (if (xor (door-lockstatus door) (ormap (λ (x) (= 2 (keycard-access-level x))) (filter keycard? (my-inventory))))
        (display-line "This door seems to be locked.")
        (begin (move! me (door-destination door))
           (look))))))


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
  (;; hunger: person's hunger level
   hunger
   ;; thirst: how thirsty the person is
   thirst

  ;; hp: player's health points
   hp
   ;; disguised? set to true while the player wears a disguise prop
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
                         100
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
(define (new-prop description examine-text location)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define prop (make-prop adjectives '() location true noun examine-text))]
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
   ;; 1 = student or faculty (limited access with certain privilege)
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
     "A purple Wildcard with a picture and a name: "
     (keycard-owner keycard))))
)

;; new-keycard: string, number, container, container -> keycard
;; Makes a new keycard with the specified parameters.
(define (new-keycard owner access-level privilege location)
  (local [(define adjs (string->words (string-append "wildcard with a name " owner)))
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


(define-struct (food prop)
  (satiety)
  #:methods
  (define (eat food)
    (if (not (empty? (container-contents food)))
        (begin
          (display-line (string-append "Wow! there's " (description (first (container-contents food))) " in here!"))
          (move! (first (container-contents food)) me)
          (display-line (string-append "You took" (description (last (container-contents me))) ". Your hunger level stays the same.")))
        (if (< (person-hunger me) 1)
            (display-line "You're not hungry anymore.")
            (begin
              (destroy! food)
              (display-line "Tasty!")
              (if (< (- (person-hunger me) (food-satiety food)) 0)
                  (begin (set-person-hunger! me 0)
                         (display-line "You're full!")
                         )
                  (set-person-hunger! me (-
                                          (person-hunger me)
                                          (food-satiety food))))
              (display-line (string-append "Your hunger level is "
                                           (number->string (person-hunger me)))))))))

(define (new-food description location examine-text satiety)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define food (make-food adjectives '() location true noun examine-text satiety))]
    (begin (initialize-thing! food)
           food)))

(define-struct (beverage prop)
  (satiety)
  #:methods
  (define (drink o)
            
        (if (< (person-thirst me) 1)
            (display-line "You're not thirsty anymore.")
            (begin
              (destroy! o)
              (display-line "mmm, thirst-quenching!")
              (if (< (- (person-thirst me) (beverage-satiety o)) 0)
                  (set-person-thirst! me 0)
                  (set-person-thirst! me (-
                                          (person-thirst me)
                                          (beverage-satiety o))))
              (display-line (string-append "your thirst level is "
                                           (number->string (person-thirst me))))))))

(define (new-beverage description location examine-text satiety)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define beverage (make-beverage adjectives '() location true noun examine-text satiety))]
    (begin (initialize-thing! beverage)
           beverage)))



(define-struct (storage prop)
  ()

  #:methods
  (define (open storage)
    (begin
      (update-stats)
      (describe-contents storage)
      (move! me (the storage))
      (newline)
      (display-line "Close the object with (close)"))))

(define (new-storage description location bool examine-text)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define storage (make-storage adjectives '() location bool noun examine-text))]
    (begin (initialize-thing! storage)
           storage)))

(define-struct (disguise prop)
  ()

  #:methods
  (define (wear x)
    (unless (person-disguised? me)
    (if (equal? (thing-location x) me)
        (begin
          (set-person-disguised?! me true)
          (display-line "You are now disguised."))
        (begin
          (take x)
          (set-person-disguised?! me true)
          (display-line "You are now disguised."))))))

  (define (new-disguise description location examine-text)
    (local [(define words (string->words description))
            (define noun (last words))
            (define adjectives (drop-right words 1))
            (define disguise (make-disguise adjectives '() location true noun examine-text))]
      (begin (initialize-thing! disguise)
             disguise)))
          
    
    
  
      
    



;;;
;;; LAPTOP
;;; Laptop: allows hacking.
;;;
(define-struct (laptop thing)
  ;; batterylevel: integer number from 0 to 100
  ;; How much battery is left.
  (batterylevel)

   #:methods
  ;; hide the noun.
  (define (noun laptop)
    "")
  ;; change examine to return keycard description.
  (define (examine laptop)
    (if (= 0 (laptop-batterylevel laptop))
    (display-line "A Linix laptop with a Northwestern sticker on the front. The battery seems dead.")
    (if (= 100 (laptop-batterylevel laptop))
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
          (define laptopcharger (make-laptopcharger adjs '() location #false))]
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
  (begin (printf "You are in ~A.~%"
                 (description (here)))
         (describe-contents (here))
         (void)))

(define-user-command (look) "Prints what you can see in the room")

(define-user-command (hack thing) "try to hack the thing")

(define (inventory)
  (if (empty? (my-inventory))
      (printf "You don't have anything.~%")
      (begin (printf "You have:~%")
             (for-each print-description (my-inventory)))))

(define-user-command (inventory)
  "Prints the things you are carrying with you.")

(define-user-command (examine thing)
  "Takes a closer look at the thing")

(define (take thing)
  (begin
    (update-stats)
    (if (thing-takable? thing)
      (begin
        (move! thing me))
      (display-line "You can't take that."))))

(define-user-command (take thing)
  "Moves thing to your inventory")

(define (drop thing)
  (move! thing (here)))

(define-user-command (drop thing)
  "Removes thing from your inventory and places it in the room
")

(define (put thing container)
  (move! thing container))

(define-user-command (put thing container)
  "Moves the thing from its current location and puts it in the container.")

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


;; check-hunger: displays player's hunger level
(define (check-hunger)
  (person-hunger me))

(define-user-command (check-hunger)
  "Displays the player's hunger level.")

;; check-thirst: displays player's thirst level
(define (check-thirst)
  (person-thirst me))

(define-user-command (check-thirst)
  "Displays the player's thirst level.")

(define (close)
  (if (storage? (thing-location me))
      (begin
        (display-line (string-append "You closed the " (description (thing-location me))))
        (move! me (thing-location (thing-location me))))
      (display-line "You haven't opened a storage object.")))

(define-user-command (open storage)
  "Opens a storage object, giving the player access to its contents.")

(define-user-command (close)
  "Closes a storage object the player opened.")

(define-user-command (eat food)
  "Consumes a food object, lowering the player's hunger level.")

(define-user-command (drink beverage)
  "Drinks a beverage object, lowering the player's thirst level.")

(define-user-command (wear disguise)
  "Takes a disguise object if the player hasn't already, and disguises the player")

(define (remove-disguise)
  (if (person-disguised? me)
      (begin
        (set-person-disguised?! me false)
        (display-line "You are no longer disguised."))
      (display-line "You aren't wearing a disguise.")))

(define-user-command (remove-disguise)
  "Un-disguises the player, but leaves the object in their inventory.")



   

;; hack: securitycam -> void
;; Change the status of the camera.
(define (hack securitycam)
  (if (have-a? laptop?)
      (if (ormap (λ (x) (> (laptop-batterylevel x) 0)) (filter laptop? (my-inventory)))
      (if (securitycam? securitycam)
          (begin (set-securitycam-status! securitycam #f)
                 (display-line "#     #    #     #####  #    # ####### ######  ### ")
                 (display-line "#     #   # #   #     # #   #  #       #     # ### ")
                 (display-line "#     #  #   #  #       #  #   #       #     # ### ")
                 (display-line "####### #     # #       ###    #####   #     #  #  ")
                 (display-line "#     # ####### #       #  #   #       #     #     ")
                 (display-line "#     # #     # #     # #   #  #       #     # ### ")
                 (display-line "#     # #     #  #####  #    # ####### ######  ### ")
                 )
          (display-line "You are not sure how to hack that."))
      (display-line "You realized that your laptop is out of battery."))
      (display-line "You don't have a a device that you can use to hack things with."))
  )

;; charge: -> void
;; Charge the laptop if a charger is present.
(define (charge)
  (if (have-a? laptopcharger?)
      (if (have-a-in-room? poweroutlet?)
          (begin (set-laptop-batterylevel! (the laptop) 100)
                 (display-line "Battery charged."))
          (display-line "There's no power outlet nearby."))
      (display-line "You don't know how to charge your laptop without a charger."))
)


;;;
;;; THE GAME WORLD - FILL ME IN
;;;

;; start-game: -> void
;; Recreate the player object and all the rooms and things.
(define (start-game)
  ;; Fill this in with the rooms you want
  (local [(define dorm-room (new-room "dorm"))
          (define dorm-hallway (new-room "hallway"))]
    (begin (set! me (new-person "Tommy Cat" dorm-room))

           ;; Add join commands to connect your rooms with doors
           (join! dorm-room "hallway"
                  dorm-hallway "dorm" #t)
           ;; Add code here to add things to your rooms

           (new-food "apple" dorm-room "apple" 15)
           (new-food "pie" dorm-room "pie" 40)
           (new-storage "old treasure chest"                                                    
                        dorm-room
                        false
                        "it is an old treasure chest, cool")
           (new-food "ripe yellow banana"
                     (the storage)
                     "it is a ripe yellow banana"
                     10)
           
           (new-beverage "cold glass of water"
                         dorm-room
                         "it is a cold glass of water"
                         25)
           
           (new-beverage "warm glass of water"
                         dorm-room
                         "a"
                         50)
           
           (new-disguise "fake moustache"
                         dorm-room
                         "it is a cool fake moustache"
                         )

           (new-prop "large file"
                     "What was this doing in an apple?"
                     (the apple))
           

           (new-keycard "Charles" 2 dorm-hallway dorm-room)
           (new-keycard "Tommy" 1 dorm-hallway me)
           (new-laptop "silver Banana Pro" dorm-room 1)
           (new-securitycam "black" dorm-hallway #t)
           (new-laptopcharger "black" me)
           (new-poweroutlet "white" dorm-room)
           (check-containers!)
           (void))))

;;;
;;; PUT YOUR WALKTHROUGHS HERE
;;;





;;;
;;; UTILITIES
;;;

;; bargraph: number -> string
;; Outputs a string as a hunger/thirst bar.
(define (bargraph value)
  (if (or (< value 0) (> value 20))
      (error "Illegal bargraph value")
      (display-line (list->string (append (make-list value #\■) (make-list (- 20 value) #\□))))
      )
  )


;; update-stats: updates a player's hunger and thirst when an action is performed
(define (update-stats)
  (begin
    (if (> (person-hunger me) 0)
        (set-person-hunger! me (- (person-hunger me) 1))
        (error "You died of hunger."))
    (if (> (person-hunger me) 0)
        (set-person-thirst! me (- (person-thirst me) 1))
        (go (the door))
        (error "You died of thirst."))))
    
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
;; True if the player as something satisfying predicate in their pocket.
(define (have-a? predicate)
  (ormap predicate
         (container-accessible-contents me)))

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

(start-game)
(look)