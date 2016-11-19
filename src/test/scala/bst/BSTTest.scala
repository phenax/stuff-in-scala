package bst;

import org.scalatest._

class BSTTest extends FlatSpec with Matchers {

	"The binary search tree" should "have only one node i.e. root" in {
		List() should have length 0
	}
}