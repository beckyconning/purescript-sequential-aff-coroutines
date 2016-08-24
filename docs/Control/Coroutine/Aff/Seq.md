## Module Control.Coroutine.Aff.Seq

#### `AffGetter`

``` purescript
type AffGetter a b eff = b -> Aff (avar :: AVAR | eff) a
```

#### `SeqProducer`

``` purescript
type SeqProducer a eff = Producer a (Aff (avar :: AVAR | eff)) Error
```

#### `produceSeq`

``` purescript
produceSeq :: forall a b eff. AffGetter a b eff -> (a -> b) -> b -> SeqProducer a eff
```


