
import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random



object GameOfLife extends App {

	def setAlive(gen: GameOfLifeGeneration)(i: Int, j: Int) = {
		gen.genGrid(i)(j)= gen.ALIVE_STATE;
	}

	val rows= 30;
	val columns= 30;
	val framerate= 10;
	
	val i= rows/2;
	val j= columns/2;

	val queenbee=
		(gen: GameOfLifeGeneration) => {

			setAlive(gen)(i - 1, j - 2);
			setAlive(gen)(i - 2, j - 1);
			setAlive(gen)(i - 3, j);
			setAlive(gen)(i - 2, j + 1);
			setAlive(gen)(i - 1, j + 2);

			setAlive(gen)(i, j - 1);
			setAlive(gen)(i, j);
			setAlive(gen)(i, j + 1);

			setAlive(gen)(i + 1, j - 3);
			setAlive(gen)(i + 1, j - 2);
			setAlive(gen)(i + 1, j + 2);
			setAlive(gen)(i + 1, j + 3);
		};

	val arrowThing=
		(gen: GameOfLifeGeneration) => {

			val size= 6;

			for(m <- -size to size) {
				setAlive(gen)(i + m, j - m);
			}

			setAlive(gen)(i + size, j - (size + 1));
			setAlive(gen)(i + size, j - (size + 2));
			setAlive(gen)(i + size, j - (size + 3));
			setAlive(gen)(i + size, j - (size + 4));
			setAlive(gen)(i + size, j - (size + 5));

			setAlive(gen)(i + (size + 1), j - size);
			setAlive(gen)(i + (size + 2), j - size);
			setAlive(gen)(i + (size + 3), j - size);
			setAlive(gen)(i + (size + 4), j - size);
			setAlive(gen)(i + (size + 5), j - size);

			setAlive(gen)(i - size, j + (size + 1));
			setAlive(gen)(i - (size - 1), j + (size + 1));
		};


	val carMoving=
		(gen: GameOfLifeGeneration) => {

			setAlive(gen)(i, j - 1);
			setAlive(gen)(i, j);
			setAlive(gen)(i, j + 1);
			setAlive(gen)(i, j + 2);

			setAlive(gen)(i + 1, j - 2);
			setAlive(gen)(i + 3, j - 2);

			setAlive(gen)(i + 1, j + 2);
			setAlive(gen)(i + 2, j + 2);
			setAlive(gen)(i + 3, j + 1);
		};


	val game= new GameOfLifeRunner(framerate, rows, columns);

	game.initialState_=(carMoving);

	game.start();
}


/**
 * Game of life generator
 * 
 * @param framerate: Int  The number of frames to render in one second
 */
class GameOfLifeRunner(framerate: Int, rows: Int, cols: Int) {

	private val CLEAR_SCREEN_CODE= "\u001b[H\u001b[2J";


	// The generations
	private var _thisGen: GameOfLifeGeneration= _;
	private var _prevGen: GameOfLifeGeneration= _;

	private var _initialState: GameOfLifeGeneration => Unit= _; 


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

	def initialState_=(stateSetter: GameOfLifeGeneration => Unit) {

		this._initialState= stateSetter;

		this._setup();
	}

	private def _setup() {

		this._prevGen= new GameOfLifeGeneration(rows, cols);
		
		this._initialState(this._prevGen);

		this._prevGen.renderGrid();
	}

	def start() {

		// Asynchronous calculation loop
		val f = Future {
			requestNextFrame(
				this.calculationLoop(rows, cols)
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

		_thisGen.inheritFrom(_prevGen);

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

	def lifeRules(live: Int, gen: GameOfLifeGeneration, row: Int, col: Int): Boolean = {

		if(gen.genGrid(row)(col) == gen.ALIVE_STATE) {
			if(live == 2 || live == 3) {
				return true;
			}
		} else if(live == 3) {
			return true;
		}
		
		return false;
	}

	// Inherit cell properties from the previous generation
	def inheritFrom(prevGen: GameOfLifeGeneration) {

		for(i <- 0 until rows) {

			for(j <- 0 until cols) {

				val numOfLiveCells= prevGen.getNeighbourLiveCount(i, j);

				val nextCellState= this.lifeRules(numOfLiveCells, prevGen, i, j);

				this.genGrid(i)(j)= 
					if(nextCellState)
						this.ALIVE_STATE
					else
						this.DEAD_STATE;
			}
		}
	}
}


