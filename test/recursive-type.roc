module []

Node : [Nil, NodeList (List Node)]

createNil : {} -> Node
createNil = \{} -> Nil

createList : {} -> Node
createList = \{} -> NodeList (List.repeat (NodeList (List.repeat Nil 0)) 0)

createNode : Bool -> Node
createNode = \bool ->
    if bool then createList {} else createNil {}

isNil : Node -> Bool
isNil = \in ->
    when in is
        Nil ->
            Bool.true

        NodeList _ ->
            Bool.false

isList : Node -> Bool
isList = \in ->
    when in is
        Nil ->
            Bool.false

        NodeList _ ->
            Bool.true

expect isNil (createNil {})
expect isList (createList {})
expect isNil (createNode Bool.false)
expect isList (createNode Bool.true)
