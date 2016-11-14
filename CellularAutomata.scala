
import scala.util.Random


object CellularAutomata extends App {

	// Start
	CARunner.init(20, 20)(

		// Anonymous function for rules
		(prevCell: Boolean, curCell: Boolean, nextCell: Boolean) => 
			if(curCell == nextCell) true else false
	);
}


object CARunner {

	// Ignore
	type B= Boolean;

	def init(row: Int, col: Int)(callback: (B, B, B) => B) {

		var thisRow: Generation= null;
		var prevRow: Generation= null;

		// Generate first random generation
		prevRow= new Generation(col);
		prevRow.fillRandomGeneration();

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
class Generation(col: Int) {

	val ALIVE: Boolean= true;
	val DEAD: Boolean= false;

	var deadChar: Char= ' ';
	var aliveChar: Char= '#';

	private var columnCount= col;
	private val column= new Array[Boolean](columnCount);
	private var current= 0;

	def get(j: Int): Boolean= {

		if(j >= this.columnCount - 1)
			return this.column(0);

		if(j < 0)
			return this.column(this.columnCount - 1);

		return this.column(j);
	}

	def add(state: Boolean) {

		if(this.current >= this.columnCount)
			return;

		this.column(this.current)= state;

		this.current += 1;
	}

	def printCell(j: Int) {

		print(
			" " + 

			(if(this.column(j)) 
				this.aliveChar 
			else
				this.deadChar)
		)
	}

	def printGeneration() {

		for(j <- 0 until columnCount)
			this.printCell(j);

		println();
	}

	def fillRandomGeneration() {

		for(cell <- this.column) {
			
			val random= Random;
			
			this.add(if(random.nextInt(2) == 1) true else false);
		}
	}
}


