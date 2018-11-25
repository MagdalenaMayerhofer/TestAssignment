///////////////////////////////////////////////////////////////////
// 1.) The big-O complexity of this function is O(2^x)
// 2.) Yes you could do better if you use dynamic Programming.
//     In this recursive function with every level you go deeper you need
//     to calculate the same solution more times. If you use dynamic
//     programming you split the function in a way, so you only need
//     to calculate each partial problem once. In that way you get a
//     big-O complexity of O(x).
//////////////////////////////////////////////////////////////////

def recursiveFunction (x: Int): Int = {
  if (x == 0) {
    1
  } else {
    recursiveFunction(x - 1) + recursiveFunction(x - 1)
  }
}

// Example of how i tested this function
recursiveFunction(8)