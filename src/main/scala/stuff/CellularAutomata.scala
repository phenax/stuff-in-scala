package stuff;

import scala.util.Random

object CellularAutomata extends App {

	// Start
	CARunner.init(20, 20)(rule3);

	// Available rules

	def rule1(prevCell: Boolean, curCell: Boolean, nextCell: Boolean): Boolean = {
		curCell == nextCell
	}

	def rule2(prevCell: Boolean, curCell: Boolean, nextCell: Boolean): Boolean = {
		// http://atlas.wolfram.com/01/01/136/
		curCell && nextCell
	}

	def rule3(prevCell: Boolean, curCell: Boolean, nextCell: Boolean): Boolean = {
		// http://atlas.wolfram.com/01/01/126/
		prevCell != curCell || curCell != nextCell
	}

	def rule4(prevCell: Boolean, curCell: Boolean, nextCell: Boolean): Boolean = {
		// http://atlas.wolfram.com/01/01/190/
		prevCell != curCell || nextCell
	}
}


object CARunner {

	// Ignore
	type B= Boolean;

	def init(row: Int, col: Int)(callback: (B, B, B) => B) {

		var thisRow: Generation= null;
		var prevRow: Generation= null;

		// Generate first random generation
		prevRow= new Generation(col);
		prevRow.randomizeGeneration();

		// Print the generation
		prevRow.printGeneration();


		for(i <- 1 until row) {

			// Create new generation
			thisRow= new Generation(col);

			for(j <- 0 until col) {

				// Generate cell based on prevRow
				val cellState= callback(
					prevRow.get(j - 1), 
					prevRow.get(j), 
					prevRow.get(j + 1)
				);

				// Give it a state of alive or dead depending 
				// on what the callback returns
				thisRow.add(
					if(cellState)
						thisRow.ALIVE
					else
						thisRow.DEAD
				);

				// Print the cell
				thisRow.printCell(j);
			}

			println();

			// On to the next one
			prevRow= thisRow;
		}
	}
}




/**
 * Each generation of cells
 * 
 * @param col: Int  Number of cells in a generation
 */
class Generation(columnCount: Int) {

	// Cell states
	val ALIVE: Boolean= true;
	val DEAD: Boolean= false;

	// Cell state representation
	var deadChar: Char= ' ';
	var aliveChar: Char= '#';

	// Generation grid
	private val column= new Array[Boolean](columnCount);

	// Column size
	private var current= 0;

	/**
	 * Get the state of the cell at an index position of col
	 * 
	 * @param col  The column position to get
	 */
	def get(col: Int): Boolean = {

		if(col >= columnCount - 1)
			return this.column(0);

		if(col < 0)
			return this.column(columnCount - 1);

		return this.column(col);
	}


	/**
	 * Add a cell to the current generation
	 * 
	 * @param state The state of the cell(DOA)
	 */
	def add(state: Boolean) {

		if(this.current >= columnCount)
			return;

		this.column(this.current)= state;

		this.current += 1;
	}


	/**
	 * Print the cell state
	 * 
	 * @param  col  The column position of the cell
	 */
	def printCell(col: Int) {

		print(
			" " + 

			(if(this.column(col)) 
				this.aliveChar 
			else
				this.deadChar)
		)
	}

	/**
	 * Print the current generation
	 */
	def printGeneration() {

		for(j <- 0 until columnCount)
			this.printCell(j);

		println();
	}

	/**
	 * Generate a random row of cells
	 */
	def randomizeGeneration() {

		for(cell <- this.column) {

			val random= Random;

			this.add(if(random.nextInt(2) == 1) true else false);
		}
	}
}


