//Hayden Platt - HW5

//1. Min function
method Min(a: int, b: int) returns (c: int)
  //post conditions
  ensures c <= a && c <= b
  ensures c < a ==> a > b
  ensures c < b ==> b > a
  
{
  if(a == b) { return a; }
  else if(a < b) { return a; }
  else { return b; }
}

method TestMin()
{
  var m := Min(12,5);
  assert m == 5;
  var n := Min(23,42);
  assert n == 23;
}

//2. Array Search function
method Search(arr: array<int>, element: int) returns (idx: int)
requires arr != null; //make sure that the array has elements 
ensures 0 <= idx ==> idx < arr.Length && arr[idx] == element
ensures -1 <= idx < arr.Length;
ensures idx < 0 ==> forall k :: 0 <= k < arr.Length ==> arr[k] != element
{
  var n := 0;
  while (n < arr.Length)
  invariant 0 <= n <= arr.Length; 
  invariant forall k :: 0 <= k < n ==> arr[k] != element;
  {
    if (arr[n] == element) {
      return n;
    }
    n := n + 1;
  }
  return -1;
}

method TestSearch()
{
  var arr := new int[3];
  arr[0] := 23;
  arr[1] := 21;
  arr[2] := 22;
  var s := Search(arr, 21);
  assert s == 1;
  var t := Search(arr, 20);
  assert t == -1;
}