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
      "no header" ~: either (T.isInfixOf "looks like data") (const False)
        (rulesTemplate "b" "a" "x.csv" "2026-01-02,Coffee,-3.50\n") ~?= True,
      "not csv" ~: either (T.isInfixOf "isn't a CSV file") (const False)
        (rulesTemplate "b" "a" "x.pdf" "%PDF") ~?= True
    ]
