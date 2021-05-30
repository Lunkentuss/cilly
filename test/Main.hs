{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import {-@ HTF_TESTS @-} GraphTest
import {-@ HTF_TESTS @-} ContainerFacadeTest

main :: IO ()
main = htfMain htf_importedTests
