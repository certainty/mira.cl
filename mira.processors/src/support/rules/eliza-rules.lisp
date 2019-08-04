;; rulebook that implements the eliza-engine
(in-package :mira.processors.support.regexp-machine)

(defsynonym "i'm" ("i am"))
(defsynonym "i'd" ("i would"))
(defsynonym "i've" ("i have"))

(defsynonym "he's" ("he is"))
(defsynonym "she's" ("she is"))

(defsynonym "it's" ("it is"))

(defsynonym "you're" ("you are"))
(defsynonym "you'd" ("you would"))
(defsynonym "you've" ("you have"))
(defsynonym "ya" ("you"))

(defsynonym "we've" ("we have"))
(defsynonym "we're" ("we are"))
(defsynonym "we'd" ("we would"))

(defsynonym "they're" ("they are"))
(defsynonym "they've" ("they have"))
(defsynonym "they'd" ("they would"))


(defrule greeting 0 ()
         (:input
          ("hello|hi|(good *)?evening|good morning|greetings"))
         (:output
          ("How do you do? Please state your problem")))

(defrule computer 0 ()
         (:input
          (" computer "))
         (:output
          ("Do computers worry you?")
          ("What do you think about machines?")
          ("Why do you mention computers?")
          ("What do you think machines have to do with your problem?")))

(defrule apologize 0 ()
         (:input
          (" sorry *"))
         (:output
          ("Please don't apologize")
          ("Apologies are not necessary")
          ("What feelings do you have when you apologize?")))

(defrule remember 0 ()
         (:input
          ("i remember (.*)" (event)))
         (:do
          (remember 'remembers ($ 'event)))
         (:output
          ("Do you often think of ~A?" ($ 'event))
          ("Does thinking of ~A bring anything else to mind?" ($ 'event))
          ("Why do you recall ~A right now?" ($ 'event))
          ("What in the present situation reminds you of ~A?" ($ 'event))
          ("What is the connection between me and ~A?" ($ 'event))))

(defrule i-remember 0 ()
         (:input
          ("do you remember (.*)" (event)))
         (:output
          ("Did you think i forget ~A?" ($ 'event))
          ("Why do you think i should recall ~A now?" ($ 'event))
          ("What about ~A?" ($ 'event))
          ("You mentioned ~A" ($ 'event))))

(defrule if 0 ()
         (:input
          ("if (.*)" (premise)))
         (:output
          ("Do you really think its likely that ~A?" ($ 'premise))
          ("Do you wish that ~A ?" ($ 'premise))
          ("What do you think about ~A?" ($ 'premise))
          ("Really ~A?" ($ 'premise))))

(defrule dreamt 0 ()
         (:input
          ("i dreamt|dreamed (.*)" (dream)))
         (:output
          ("Really ~A?" ($ 'dream))
          ("Have you every fantasized ~A while you were awake?" ($ 'dream))
          ("Have you dreamt of ~A? before" ($ 'dream))))

(defrule dream-about 0 ()
         (:input
          ("dream about (.*)" (dream)))
         (:output
          ("How do you feel about ~A in reality?" ($ 'dream))))

(defrule dream 0 ()
         (:input
          ("dream (.*)" (dream)))
         (:output
          ("What does this dream suggest to you?")
          ("Do you dream often?")))


