## version 2.0-RC1

> 15.01.2020

Replace monocle with tofu-optics

## version 1.5.1

> 10.12.2019

- upgrade `reactivemongo`
- upgrade `typed-schema`

## version 1.5

> 14.11.2019

- upgrade `circe`
- upgrade `reactivemongo`
- upgrade `typed-schema`

## version 1.4

> 19.09.2019

Introduce `ToFilter` typeclass for Json filtering

## version 1.3

> 11.09.2019

Uncurry `unsafeOneOfDiscr` to avoid overload clashing

## version 1.2

> 11.09.2019

- add convenience overloads for a bunch of methods `annotated.Schema`
- bump mouse

## version 1.1.1

> 11.09.2019

- bump `monocle`

## version 1.1

> 11.09.2019

- Support schema annotations
- upgrade `reactivemongo`

## version 1.0

> 01.09.2019

Identical to 1.0-beta15

## version 1.0-beta15

> 02.08.2019

Fix `typed-schema` dependency

## version 1.0-beta14

> 06.06.2019

Improve `absent` field constructor

## version 1.0-beta13

> 06.06.2019

Introduce `absent` for representing properties that are always absent

## version 1.0-beta12

> 31.05.2019

Introduce `unsafeOneOfDiscr`

## version 1.0-beta11

> 30.05.2019

PropSchema construction methods now accept `Lens`

## version 1.0-beta10

> 30.05.2019

Improve `constant` field constructor

## version 1.0-beta9

> 29.05.2019

- upgrade `typed-schema`
- upgrade `kind-projector`

## version 1.0-beta8

> 27.05.2019

Display discriminator values in swagger as patterns

## version 1.0-beta7

> 06.05.2019

First-class support for constant fields in schemas

## version 1.0-beta6

> 18.04.2019

- rename `toTypeable` into `typeable`
- rename `toGen` into `gen` 

## version 1.0-beta5

> 17.04.2019

Circe module can produce AccumulatedDecoders

## version 1.0-beta4

> 14.04.2019

Minor fix in reactivemongo module

## version 1.0-beta3

> 11.04.2019

Support for default values of required properties

## Version 1.0-beta2

> 01.04.2019

Support for OneOf schemas with discriminator fields

## Version 1.0-beta1

> 31.03.2019

Initial release with support of
- circe
- reactivemongo
- scalacheck
- typed-schema