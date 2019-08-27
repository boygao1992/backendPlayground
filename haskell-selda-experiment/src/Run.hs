{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Run (run) where

import Import

import Database.Selda
import Database.Selda.MakeSelectors
import Database.Selda.SQL.Print
import Database.Selda.SQL.Print.Config
import Database.Selda.Query.Type

data Person = Person
  { id :: ID Person
  , name :: Text
  , age :: Int
  , pet :: Maybe Text
  , cash :: Double
  } deriving (Generic, SqlRow)

people :: Table Person
people = table "people"
  [ #id   :- primary
  , #name :- index
  ]

peopleItems :: [Person]
peopleItems =
  [ Person def def def def def
  ]

addresses :: Table (Text, Text)
addresses = table "addresses" []
aCity :: Selector (Text, Text) Text
aName :: Selector (Text, Text) Text
aName :*: aCity = selectors addresses

comments :: Table (RowID, Maybe Text, Text)
comments = table "comments" [ cId :- untypedAutoPrimary ]

cId :: Selector (RowID, Maybe Text, Text) RowID
cComment :: Selector (RowID, Maybe Text, Text) Text
cName :: Selector (RowID, Maybe Text, Text) (Maybe Text)
cId :*: cName :*: cComment = selectors comments

leftJoinThenProduct :: Query s (Col s Text :*: Col s (Maybe Text) :*: Col s Text)
leftJoinThenProduct = do
  name :: Col s Text
    <- #name `from` select people
  addressName :: Col s (Maybe Text)
    <- leftJoin (\a -> just name .== a) $
    do
      addressName :: Col (Inner s) Text
        <- aName `from` select addresses
      commentName <- leftJoin (\c -> just addressName .== c) $ do
        commentName :: Col (Inner (Inner s)) (Maybe Text)
          <- cName `from` select comments
        pure commentName
      pure commentName
  c <- select comments
  restrict $ c!cName .== just name
  pure $ name :*: addressName :*: c!cComment

{-
SELECT
  "col_0_12", "col_1_13", "col_2_14", "col_1_8_10_11"
  , "id_0", "name_1", "age_2", "pet_3", "cash_4"
FROM
  ( SELECT "col_1_8_10_11", "id_0", "name_1", "age_2", "pet_3", "cash_4"
    FROM
      ( SELECT "id"   AS "id_0"
             , "name" AS "name_1"
             , "age"  AS "age_2"
             , "pet"  AS "pet_3"
             , "cash" AS "cash_4"
        FROM "people"
      ) AS q4
      LEFT JOIN
      ( SELECT "col_1_8_10" AS "col_1_8_10_11"
        FROM
          ( SELECT "col_1_8_10", "col_0_5", "col_1_6"
            FROM
              ( SELECT "col_0" AS "col_0_5"
                     , "col_1" AS "col_1_6"
                FROM "addresses"
              ) AS q1
              LEFT JOIN
              ( SELECT
                  "col_1_8" AS "col_1_8_10"
                FROM
                  ( SELECT "col_0" AS "col_0_7"
                         , "col_1" AS "col_1_8"
                         , "col_2" AS "col_2_9"
                    FROM "comments"
                  ) AS q0
              ) AS q2
              ON (CAST("col_0_5" AS TEXT)) = "col_1_8_10") AS q3
      ) AS q5
      ON (CAST("name_1" AS TEXT)) = "col_1_8_10_11"
  ) AS q6
  ,
  ( SELECT "col_0" AS "col_0_12"
         , "col_1" AS "col_1_13"
         , "col_2" AS "col_2_14"
    FROM "comments"
  ) AS q7
WHERE ("col_1_13" = (CAST("name_1" AS TEXT)))
-}

run :: RIO App ()
run = do
  logInfo "We're inside the application!"

  logInfo $ displayShow $ compSql defPPConfig $ head $ sources $ snd $ runQueryM 0 leftJoinThenProduct
