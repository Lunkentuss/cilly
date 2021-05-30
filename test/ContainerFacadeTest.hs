{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ContainerFacadeTest (htf_thisModulesTests) where

import Test.Framework

import ContainerFacade

test_sanitize_A1 = assertEqual (sanitizeContainerName "abc_.-") "abc_.-"
test_sanitize_A2 = assertEqual (sanitizeContainerName "___") "0__"
test_sanitize_A3 = assertEqual (sanitizeContainerName "@a/}b#") "0a__b_"
test_sanitize_A4 = assertEqual (sanitizeContainerName "-") "0"
test_sanitize_A5 = assertEqual (sanitizeContainerName ".") "0"
test_sanitize_A6 = assertEqual (sanitizeContainerName "_") "0"
test_sanitize_A7 = assertEqual (sanitizeContainerName "") ""
