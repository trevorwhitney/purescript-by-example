module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
import Data.Maybe (Maybe)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

{-- <> is used for string concatenation --}
showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

{--
Point free evolution of insertEntry:
insertEntry entry is a function whose argument is just passed along to the (Cons entry) function.
But if two functions have the same result for every input, then they are the same function!

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = (Cons entry) book
insertEntry entry = Cons entry
insertEntry = Cons

--}
insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

filterEntry :: String -> String -> Entry -> Boolean
filterEntry firstName lastName entry = 
  entry.firstName == firstName 
  && entry.lastName == lastName

{-- <<< is "backwards composition" for composing function. >>> is forwards, ie. filter filterEntry >>> head --}
findEntry :: String → String → AddressBook -> Maybe Entry
findEntry firstName lastName = filterHead
  where
    filterHead :: AddressBook -> Maybe Entry
    filterHead = head <<< filter (filterEntry firstName lastName)

isNameInBook :: String -> String -> AddressBook -> Boolean
isNameInBook firstName lastName = not null <<< filter (filterEntry firstName lastName)

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy compareEntries
  where
    compareEntries :: Entry -> Entry -> Boolean
    compareEntries entry = filterEntry entry.firstName entry.lastName
