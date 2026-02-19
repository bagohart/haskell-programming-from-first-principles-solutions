data DoesntForce = TisLazy Int String
data BangBang = SheShotMeDown !Int !String

gibString :: DoesntForce -> String
gibString (TisLazy _ s) = s

gimmeString :: BangBang -> String
gimmeString (SheShotMeDown _ s) = s

-- Prelude> let x = TisLazy undefined "blah"
-- Prelude> gibString x
-- > "blah"
-- Prelude> let x = SheShotMeDown undefined "blah"
-- Prelude> gimmeString x
-- > "*** Exception: Prelude.undefined
