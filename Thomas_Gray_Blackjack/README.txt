# Blackjack program in Haskell
*Overview
- This project is a functional game of blackjack implemented in Haskell.
 It uses a linear congruential generator (LCG) for pseudo random number generation.
 Using the constants in rand () from C/C++.
    It supports:
        - Random Number Generation at the start of the 1st game
            - User inputs any number to be the seed, and it will generate random numbers from there
            - Note: If the user inputs the same number at the start of each game the round will play out the same
        - Drawing cards, hitting, and staying (via the prompt h/s) by consuming user input
        - Tracking and updating the player's hand total after every hit
        - Handling Busts for the player and dealer
        - Tracking Wins and Losses
        - Handling Tie's
        - Playing multiple rounds until the user enters "exit"
        - Outputs the results of the game after every rounds
        - Ace values changing when the hand total is above 11
        - Announcing the winner of each round

*File Structure:
    - Single file: blackjack.hs 

# Compliation
- Compile with: (Recommended)
        ghci blackjack.hs
    THEN start the game with: 
        main
    TO Leave ghci:
        :q OR :quit

- OR With: (Not Recommended as it has less readable prompts)
        ghc blackjack.hs
    THEN:
        ./blackjack