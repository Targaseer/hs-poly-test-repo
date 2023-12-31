module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst = 
    initReverse (reverse lst)
    where 
        initReverse [] = REmpty
        initReverse (hlst : tlst) = (initReverse tlst) :< hlst

reverseRevLst :: ReverseList a -> ReverseList a
reverseRevLst REmpty = REmpty
reverseRevLst revLst = 
    reverseRevLstAcc revLst REmpty 
    where
        reverseRevLstAcc REmpty acc = acc
        reverseRevLstAcc (rlinit :< rllast) acc = reverseRevLstAcc rlinit (acc :< rllast)

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
    show REmpty = "[]"
    show revL = 
        "[" ++ (pureStr revL) ++ "]" 
        where 
            pureStr (REmpty :< lastEl) = show lastEl
            pureStr (init :< last) = (pureStr init) ++ "," ++ (show last)
instance (Eq a) => Eq (ReverseList a) where
    REmpty == REmpty = True
    (_ :< alast) == REmpty = False
    REmpty == (_ :< blast) = False
    (ainit :< alast) == (binit :< blast) = alast == blast && ainit == binit
instance Semigroup (ReverseList a) where
    REmpty <> REmpty = REmpty
    revLst <> REmpty = revLst
    REmpty <> revLst = revLst
    revLstA <> revLstB = 
        appnd revLstA (reverseRevLst revLstB)
        where
            appnd revLstA REmpty = revLstA
            appnd revLstA (reversedRevLstBinit :< reversedRevLstBlast) = appnd (revLstA :< reversedRevLstBlast) reversedRevLstBinit
instance Monoid (ReverseList a) where
    mempty = REmpty
instance Functor ReverseList where
    fmap f (REmpty) = REmpty
    fmap f (revLstInit :< revLstLast) = (fmap f revLstInit) :< (f revLstLast)
instance Applicative ReverseList where
    pure a = REmpty :< a
    REmpty <*> _ = REmpty
    _ <*> REmpty = REmpty
    funcsRevLst <*> revLst = 
        applyF (reverseRevLst funcsRevLst) revLst REmpty
        where
            applyF REmpty _ acc = acc
            applyF (funcsRevLstInit :< funcsRevLstLast) revLst acc = applyF funcsRevLstInit revLst (acc <> (fmap funcsRevLstLast revLst))
instance Monad ReverseList where
    revLst >>= funcToRevLst = 
        flattenRevLsts (reverseRevLst (fmap funcToRevLst revLst)) REmpty
        where
            flattenRevLsts REmpty acc = acc
            flattenRevLsts (packedRevListsInit :< packedRevListsLast) acc = flattenRevLsts packedRevListsInit (acc <> packedRevListsLast)