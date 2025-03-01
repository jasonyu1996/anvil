### Data Types

#### Data Type Expressions

Data types can be described using data type expressions:
```
data-type-expression ::= () | logic | $identifier | ($data-type-expression {,$data-type-expression}) | ( $data-type-expression [ {digit}+ ] )
```

#### Data Type Definitions

Data types described by data type expressions can be given names in data type definitions:
```
data-type-definition ::= type $identifier [ $params ] = $data-type-expression ;
```

#### Struct Definitions

A named struct type can be defined:
```
struct $identifier [ $params ] { $identifier : $data-type-expression {, $identifier : $data-type-expression} }
```

#### Enum Definitions

A named enum type can be defined:
```
enum $identifier { $identifier {, $identifier} }
```

