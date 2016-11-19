package bst;

class BSTNode[T](value: T) {
	
	private val _value: T= value;

	private var _leftNode: BSTNode[T]= null;
	private var _rightNode: BSTNode[T]= null;

	// Value getter
	def value(): T= value;

	// Node getters
	def leftNode()=  _leftNode;
	def rightNode()= _rightNode;

	// Node value getters
	def left()=  _leftNode.value();
	def right()= _rightNode.value();

	// Node setters
	def left_=(_left: BSTNode[T])= 
		this._leftNode= _left;

	def right_=(_right: BSTNode[T])= 
		this._rightNode= _right;

	def isTail() = (_leftNode == null && _rightNode == null);

	override def toString()= "---" + value.toString();
}
