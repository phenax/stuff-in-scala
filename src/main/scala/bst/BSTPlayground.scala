package bst;

object BSTPlayground extends App {
	
	val tree= new BinarySearchTree[Int]();

	tree.add(3);
	tree.add(5);
	tree.add(2);
	tree.add(3);
	tree.add(8);

	tree.print();
}