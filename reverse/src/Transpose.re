open CS17setup;

/* Type signature 
transpose: (list (list (int))) -> (list (list (int))) */

/* Inputs: a list of lists of ints, mat
   Output: a new list of lists of ints, the individual elements of mat reversed*/

/*recursive diagrams
OI: [[1,2].[3,4]]
  RI: [[2],[4]]
  RO: [[2,4]]
appends the flattened list to the flattened list of the heads of the element lists 
OO: [[1,3], [2,4]]
*/

type matrix('a) = list(list('a));

let rec transpose : matrix('a) => matrix('a) = mat => 
switch (mat) {
| [] => []
| [[], ..._] => failwith("A matrix cannot be 0-dimensional.")
| [[hd], ..._] => [List.flatten(mat)]
| [[hd, ...tl], ..._] => [List.map(List.hd, mat), ...transpose(List.map(List.tl, mat))]
};

       
 check_expect_list_list_alpha(transpose([[1,2,3], [4,5,6],[7,8,9]]), [[1,4,7],[2,5,8],[3,6,9]], "multi element list");
 check_expect_list_list_alpha(transpose([[1,2], [3,4]]), [[1,3],[2,4]], "second multi element list");