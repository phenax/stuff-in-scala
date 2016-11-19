package bst;

import scala.util.Random

object BSTPlayground extends App {
	
	val tree= new BinarySearchTree[Int]();

	val random= Random

	for(i <- 0 to 10) {
		tree.add(random.nextInt(100));
	}

	tree.add(54);

	tree.printSearchPath(54);

	// tree.printTree();
}