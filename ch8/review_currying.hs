cattyConny x y = x ++ " mrow " ++ y
appedCatty = cattyConny "woops"

flippy = flip cattyConny
frappe = flippy "haha"

-- 1. appedCatty "woohoo!" = "woops mrow woohoo!"
-- 2. frappe "1" = "1 mrow haha"
-- ...
