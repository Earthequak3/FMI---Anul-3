data Piece = One   -- primul jucător
            | Two   -- al doilea jucător
            | Empty -- căsuță liberă pe tablă
    deriving (Show, Eq)

data Table = Table [Piece] [Piece] [Piece]
    deriving (Show,Eq)

table1 :: Table
table1 = Table [Empty, One, Two, One, Empty, Empty, Two, One]
       [Two, Empty, One, Two, One, Two, One, Two]
       [Two, Two, One, Empty, Empty, One, Two, One]

table2 :: Table
table2 = Table [Two, One, Two, One, Empty, Empty, Two, One]
       [Two, Empty, One, Two, One, Two, One, Two]
       [Two, Two, One, Empty, Empty, One, Two, One]

table3 :: Table
table3 = Table [Empty, One, Empty, Empty, Empty, Empty, Two, One]
       [Two, Empty, One, Two, One, Two, One, Two]
       [Two, Empty, One, Empty, Empty, One, Two, One]

table4 :: Table
table4 = Table [Empty,Empty,Two,One,Empty,Empty,Two,One]
       [Two,One,One,Two,One,Two,One,Two]
       [Two,Two,One,Empty,Empty,One,Two,One]