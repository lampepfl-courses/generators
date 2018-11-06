import generators._
import Generator._

// This test is true
test(lists) {
  xs => xs.reverse.reverse == xs
}
// This test will fail for two empty lists
test(pairs(lists, lists)) {
  case (xs, ys) => (xs ++ ys).length > xs.length
}
