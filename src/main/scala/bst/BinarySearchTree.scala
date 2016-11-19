package bst;

class BinarySearchTree[T <% Ordered[T]] {

	private var _root: BSTNode[T]= null;

	def add(value: T): BinarySearchTree[T] = {

		if(_root == null) {
			_root= new BSTNode[T](value);
		} else {

			val newNode= new BSTNode[T](value);

			this.traverse((node) => {

				if(value > _root.value()) {

					if(_root.rightNode() == null) {
						_root.right_=(newNode);
						0;
					}
					
					1;
				} else {

					if(_root.leftNode() == null) {
						_root.left_=(newNode);
						0;
					}
					
					-1;
				}

				0;
			});
		}

		return this;
	}

	def print() = foreach((i, node) => { println("[" + i + "]" + node.value()); true; });

	def traverse(callback: BSTNode[T] => Int): BinarySearchTree[T] = {

		def _iterator(node: BSTNode[T]) {

			val direction: Int= callback(node);

			if(direction == -1 && node.leftNode() != null)
				_iterator(node.leftNode());
			else if(direction == 1 && node.rightNode() != null)
				_iterator(node.rightNode());
		}

		_iterator(_root);

		return this;
	}

	def foreach(callback: (Int, BSTNode[T]) => Boolean): BinarySearchTree[T] = {

		var index= 0;

		def _forEach(node: BSTNode[T]) {

			if(callback(index, node)) {

				index += 1;

				if(node.leftNode() != null)
					_forEach(node.leftNode());

				if(node.rightNode() != null)
					_forEach(node.rightNode());
			}
		}

		_forEach(_root);

		return this;
	}

}
