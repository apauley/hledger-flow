{-# LANGUAGE OverloadedStrings #-}

module RulesTemplate.Unit (tests) where

import qualified Data.Text as T
import Hledger.Flow.RulesTemplate
import Test.HUnit

tests :: Test
tests =
  TestList
    [ "bom stripped" ~: templateFields (snd (splitHeader "\xFEFF\&Date,Desc,Amount")) ~?= ["date", "description", "amount"],
      "semicolons" ~: fst (splitHeader "\"Datum\";\"Omschrijving\";\"Bedrag\"") ~?= ';',
      "tabs" ~: fst (splitHeader "Date\tDesc\tAmount") ~?= '\t',
      "mapping" ~: templateFields ["Transaction Date", "Payee Name", "Amount", "Balance (ZAR)", "", "Amount"]
        ~?= ["date", "description", "amount", "balance_zar", "field5", "bank_amount"],
      "hledger's special names renamed" ~: templateFields ["Date", "Description", "Amount", "Balance", "Status", "Code"]
        ~?= ["date", "description", "amount", "bank_balance", "bank_status", "bank_code"],
      "single amount column mapped" ~: templateFields ["Date", "Desc", "Transaction Amount"] ~?= ["date", "description", "amount"],
      "suffixes are unique even when present in the header" ~:
        templateFields ["Reference", "Reference", "Reference_2", "Reference"]
        ~?= ["reference", "reference_2", "reference_2_2", "reference_3"],
      "skip a suffix already used by an earlier column" ~:
        templateFields ["Reference_2", "Reference", "Reference"] ~?= ["reference_2", "reference", "reference_3"],
      "debit and credit amounts left alone" ~: templateFields ["Date", "Desc", "Debit Amount", "Credit Amount"]
        ~?= ["date", "description", "debit_amount", "credit_amount"],
      "renamed columns explained" ~: fmap (T.isInfixOf "bank_balance")
        (rulesTemplate "b" "a" "x.csv" "Date,Desc,Amount,Balance\n")
        ~?= Right True,
      "data row" ~: looksLikeHeader ["2026-01-02", "Coffee", "-3.50"] ~?= False,
      "header row" ~: looksLikeHeader ["Date", "Desc", "Amount"] ~?= True,
      "template fields" ~: fmap (T.isInfixOf "fields date, description, amount\n")
        (rulesTemplate "mybank" "checking" "import/me/mybank/checking/1-in/2026/jan.csv" "Date,Desc,Amount\n1,2,3\n")
        ~?= Right True,
      "template accounts" ~: fmap (T.isInfixOf "account1 assets:mybank:checking\n")
        (rulesTemplate "mybank" "checking" "x.csv" "Date,Desc,Amount\n")
        ~?= Right True,
      "template separator" ~: fmap (T.isInfixOf "separator ;\n")
        (rulesTemplate "b" "a" "x.csv" "Date;Desc;Amount\n")
        ~?= Right True,
      "no separator line for commas" ~: fmap (T.isInfixOf "separator")
        (rulesTemplate "b" "a" "x.csv" "Date,Desc,Amount\n")
        ~?= Right False,
      "no header" ~: fmap (T.isInfixOf "fields date, column2, amount")
        (rulesTemplate "b" "a" "x.csv" "2026-01-02,Coffee,-3.50\n") ~?= Right True,
      "Wells Fargo starter" ~:
        fmap (\t -> all (`T.isInfixOf` t)
          ["#   column1 = 11/14/2026", "#   column2 = -4.50", "#   column3 = *", "#   column4 =", "#   column5 = COFFEE SHOP",
           "fields date, amount, column3, column4, description", "date-format %m/%d/%Y",
           "account1 assets:wf:bchecking", "account2 expenses:unknown"] && not ("skip " `T.isInfixOf` t))
          (rulesTemplate "wf" "bchecking" "x.csv" "11/14/2026,-4.50,*,,COFFEE SHOP\n11/15/2026,-6.75,*,,BOOK SHOP\n")
        ~?= Right True,
      "amount and balance stay neutral" ~: fmap (T.isInfixOf "fields date, column2, column3, description")
        (rulesTemplate "b" "a" "x.csv" "2026-01-02,-5,100,Coffee\n2026-01-03,10,110,Shop\n") ~?= Right True,
      "ambiguous day and month are commented" ~: fmap (\t -> "# date-format" `T.isInfixOf` t && not ("\ndate-format" `T.isInfixOf` t))
        (rulesTemplate "b" "a" "x.csv" "01/02/2026,-5,Coffee\n01/03/2026,-10,Shop\n") ~?= Right True,
      "mixed date separators are commented" ~: fmap (T.isInfixOf "# date-format")
        (rulesTemplate "b" "a" "x.csv" "12/31/2026,-5,Coffee\n30.12.2026,-10,Shop\n") ~?= Right True,
      "multiline sample stays commented" ~: fmap (T.isInfixOf "#     account1 other")
        (rulesTemplate "b" "a" "x.csv" "2026-01-01,\"Coffee, shop\naccount1 other\",-5\n2026-01-02,Store,-2\n") ~?= Right True,
      "read only fifty data records" ~: fmap (T.isInfixOf "fields date, amount, column3")
        (rulesTemplate "b" "a" "x.csv" (T.replicate 50 "2026-01-01,-5,Store\n" <> "2026-01-02,bad,Diff\n")) ~?= Right True,
      "ragged sample refuses" ~: either (const True) (const False)
        (rulesTemplate "b" "a" "x.csv" "2026-01-01,-5,Store\n2026-01-02,-2\n") ~?= True,
      "not csv" ~: either (T.isInfixOf "isn't a CSV file") (const False)
        (rulesTemplate "b" "a" "x.pdf" "%PDF") ~?= True
    ]
