## Module Control.Coroutine.Aff.Seq

#### `AffGetter`

``` purescript
type AffGetter a b eff = b -> Aff (avar :: AVAR | eff) a
```

#### `produceSeq`

``` purescript
produceSeq :: forall a b m eff. (Affable (avar :: AVAR | eff) m, Functor m) => AffGetter a b eff -> (a -> b) -> b -> Producer a m Unit
```

#### `produceAff'`

``` purescript
produceAff' :: forall a r m eff. (Affable (avar :: AVAR | eff) m, Functor m) => ((Either a r -> Aff (avar :: AVAR | eff) Unit) -> Aff (avar :: AVAR | eff) Unit) -> Producer a m r
```


