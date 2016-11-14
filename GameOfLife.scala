
import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random



object GameOfLife extends App {

	// The rule for the game
	val golRules= 
		(live: Int, gen: GameOfLifeGeneration, row: Int, col: Int) => {

			if(gen.genGrid(row)(col) == gen.ALIVE_STATE && live == 3) {
				true;
			} else if(live == 2 || live == 3) {
				true;
			} else {
				false;
			}
		};


	val game= new GameOfLifeRunner(10);

	// Set the rules callback
	game.rules_=(golRules);
	game.start(50, 80);
}


/**
 * Game of life generator
 * 
 * @param framerate: Int  The number of frames to render in one second
 */
class GameOfLifeRunner(framerate: Int) {

	private val CLEAR_SCREEN_CODE= "\u001b[H\u001b[2J";


	// The generations
	private var _thisGen: GameOfLifeGeneration= _;
	private var _prevGen: GameOfLifeGeneration= _;

	// Rules
	private var _rules: (Int, GameOfLifeGeneration, Int, Int) => Boolean = _;


	// Put the current thread to sleep for `time`ms
	def sleep(time: Int) = Thread.sleep(time);

	// Clears the terminal screen
	def clearScreen() = println(CLEAR_SCREEN_CODE);

	// Execute after frame delay
	def requestNextFrame(callback: ()=> Unit) {

		// 16 second sleep for 60fps
		sleep((1000/framerate).toInt);

		callback();
	}

	def rules_=(callback: (Int, GameOfLifeGeneration, Int, Int) => Boolean) {
		this._rules= callback;
	}

	def start(row: Int, col: Int) {

		_prevGen= new GameOfLifeGeneration(row, col);
		_prevGen.setFirstGeneration();

		// Asynchronous calculation loop
		val f = Future {
			requestNextFrame(
				this.calculationLoop(row, col)
			);
		}

		// On the main process so that it doesnt exit out
		// Need to skip a frame to render to make sure the calculations are complete
		requestNextFrame(
			() => requestNextFrame(
				this.runRenderLoop()
			)
		);
	}

	def runRenderLoop()() {

		this.clearScreen();

		// Render the stuff to stdout
		_thisGen.renderGrid();

		requestNextFrame(
			this.runRenderLoop()
		);
	}

	def calculationLoop(row: Int, col: Int)() {

		_thisGen= new GameOfLifeGeneration(row, col);

		_thisGen.inheritFrom(_prevGen)(this._rules);

		_prevGen= _thisGen;

		requestNextFrame(
			this.calculationLoop(row, col)
		);
	}
}




/**
 * Each generation of the game i.e. each frame
 * 
 * @param rows: Int  Number of rows in the generation grid
 * @param cols: Int  Number of cols in the generation grid
 */
class GameOfLifeGeneration(rows: Int, cols: Int) {

	// The two cell states
	val ALIVE_STATE = '#';
	val DEAD_STATE  = '-';

	// The generation grid
	val genGrid= Array.fill[Char](rows, cols) { DEAD_STATE };

	// Render the grid to the terminal
	def renderGrid() {

		// Loop through the row
		for(row <- this.genGrid) {

			// Loop through the columns
			for(col <- row) {

				// Render the cell state
				print(col + " ");
			}

			println();
		}
	}

	// Set the first generation of cells
	def setFirstGeneration() {

		val random= new Random();

		this.genGrid(rows/2)(cols/2)= ALIVE_STATE;
		this.genGrid(rows/2 - 1)(cols/2 - 1)= ALIVE_STATE;
		this.genGrid(rows/2 - 1)(cols/2)= ALIVE_STATE;
		this.genGrid(rows/2 - 1)(cols/2 + 1)= ALIVE_STATE;

		// for(i <- 0 until rows) {

		// 	for(j <- 0 until cols) {

		// 		this.genGrid(i)(j)= 
		// 			if(random.nextInt(10) == 0)
		// 				this.ALIVE_STATE
		// 			else
		// 				this.DEAD_STATE;
		// 	}
		// }
	}

	def getNeighbourLiveCount(indexI: Int, indexJ: Int): Int = {

		var numOfLiveCells= 0;

		var m= 0;
		var n= 0;

		for(i <- -1 to 1) {
			
			m= indexI + i;

			for(j <- -1 to 1) {

				n= indexJ + j;

				if(i != 0 || j != 0) {
					
					if(m >= rows)
						m %= rows;
					 else if(m < 0)
						m = rows + m;

					if(n >= cols)
						n %= cols;
					 else if(n < 0)
						n = cols + n;

					if(this.genGrid(m)(n) == ALIVE_STATE)
						numOfLiveCells += 1;
				}
			}
		}

		return numOfLiveCells;
	}

	// Inherit cell properties from the previous generation with the help of rules
	def inheritFrom(prevGen: GameOfLifeGeneration)(rules: (Int, GameOfLifeGeneration, Int, Int) => Boolean) {

		for(i <- 0 until rows) {

			for(j <- 0 until cols) {

				val numOfLiveCells= prevGen.getNeighbourLiveCount(i, j);

				val nextCellState= rules(numOfLiveCells, prevGen, i, j);

				this.genGrid(i)(j)= 
					if(nextCellState)
						this.ALIVE_STATE
					else
						this.DEAD_STATE;
			}
		}
	}
}


