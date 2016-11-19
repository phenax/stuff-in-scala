package bst;

import scala.collection.mutable.ArrayBuffer;

class BinarySearchTree[T <% Ordered[T]] {

	private var _root: BSTNode[T]= null;


	/**
	 * Adds a new node to the BST
	 * 
	 * @param value  The value of the BST node
	 */
	def add(value: T): BinarySearchTree[T] = {

		val newNode= new BSTNode[T](value);

		if(_root == null) {
			_root= newNode;
		} else {

			this.traverse((node) => {

				if(value > node.value()) {

					if(node.rightNode() == null) {
						node.right_= (newNode);
						0;
					} else 1;
				} else {

					if(node.leftNode() == null) {
						node.left_= (newNode);
						0;
					} else -1;
				}
			});
		}

		return this;
	}



	/**
	 * Prints the BST to the terminal
	 */
	def printTree() = 
		foreach(
			(i, node, side) => {
				
				println(" [" + i + "]\t" + side + "\t" + node);
				
				true;
			}
		);


	/**
	 * Traverses the binary tree
	 * 
	 * @param callback  For data about what node to traverse to next
	 *                      if returns -1, go left
	 *                      if returns 1, go right
	 *                      else stop
	 * 
	 * @return          The list of paths that the callback took
	 */
	def traverse(callback: BSTNode[T] => Int): ArrayBuffer[List[Any]] = {

		val myList= ArrayBuffer.empty[List[Any]];

		def _iterate(node: BSTNode[T]) {

			val direction: Int= callback(node);

			myList += List(node, direction);

			if(direction == -1 && node.leftNode() != null)
				_iterate(node.leftNode());
			else if(direction == 1 && node.rightNode() != null)
				_iterate(node.rightNode());
		}

		_iterate(_root);

		return myList;
	}


	/**
	 * Iterates through all nodes in the BST and calls the callback
	 * 
	 * @param  callback   Is called at each node
	 *                      - Gets (index, node, direction)
	 *                      - if returns true, keep true
	 *                           else stop iterating
	 */
	def foreach(callback: (Int, BSTNode[T], Int) => Boolean): BinarySearchTree[T] = {

		var index= 0;

		def _iterate(node: BSTNode[T], side: Int) {

			if(node == null) return;

			val shouldIterate: Boolean= callback(index, node, side);

			if(shouldIterate) {

				index += 1;

				// Recursive calls for both left and right nodes
				_iterate(node.leftNode(), -1);
				_iterate(node.rightNode(), 1);
			}
		}

		_iterate(_root, 0);

		return this;
	}


	/**
	 * Checks if a certain value exists in the tree
	 * 
	 * @param value  The value of the node to look for
	 *
	 * @return       True it the value is found
	 */
	def contains(value: T): Boolean = (search(value) != -1);



	/**
	 * Search for a value in the BST
	 *
	 * @param value  The value of the node to look for
	 *
	 * @return       List?
	 */
	def search(value: T): ArrayBuffer[List[Any]] = {

		var valueFound: Any= -1;

		val travPath= this.traverse((node) => {

			if(valueFound != -1) 0;

			if(value == node.value) {
				valueFound= node.value;
				0;
			} else if(value > node.value) 1 else -1;
		});

		return if(valueFound == -1) null else travPath;
	}


	def printSearchPath(value: T) = {
		
		val spaceIncr= 2;

		def printSpaces(spaces: Int) =
			for(j <- 0 until spaces)
				print(" ");


		val path= this.search(value);
		var spaces= 1;

		println();
		
		if(path == null)
			println("Nothing found");
		else {
			
			for(i <- 0 until path.length) {

				val dir= if(i == 0) 0 else path(i - 1)(1);

				if(i != 0) {
					printSpaces(spaces);
					println(if(dir == 1) " \\" else "|");
				}
				
				if(dir == 1)
					spaces+= spaceIncr;

				printSpaces(spaces);
				println(path(i)(0));
			}
		}
		
		println();
	}
}
