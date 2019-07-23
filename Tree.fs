
module Tree

  type Tree<'a> = 
    | Empty
    | Leaf of 'a
    | Node of 'a * 'a Tree * 'a Tree

  let emptyTree = Empty

  let isEmpty = 
    function 
    | Empty -> true
    | _     -> false

  let leaf x = Leaf x

  let isLeaf = 
    function
    | Leaf _ -> true
    | _ -> false

  let root left v right = Node(v, left, right)

  let head = 
    function
    | Empty -> failwith "Error @ head, head empty"
    | Leaf v -> v
    | Node(v, _, _) -> v

  let left = 
    function
    | Empty -> failwith "Error @ left, root empty"
    | Leaf _ -> failwith "Error @ left, root is leaf"
    | Node(_, left, _) -> left

  let right = 
    function
    | Empty -> failwith "Error @ right, root empty"
    | Leaf _ -> failwith "Error @ right, root is leaf"
    | Node(_, _, right) -> right