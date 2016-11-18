package stuff;

import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object GameOfLife extends App {

	// Number of rows and columns in the 2D grid
	val rows= 30;
	val columns= 30;

	// Number of frames per second
	val framerate= 10;

	// The number of frames you wanna run(-1 = Infinite)
	val numberOfFrames= -1;


	val game= new GameOfLife(framerate, rows, columns);
	val states= new GOLStates(rows/2, columns/2);

	/*
	 * Available initial states
	 *   - queenBee
	 *   - lineMagic
	 *   - birdey
	 */
	game.initialState_=(states.queenBee);

	game.start(numberOfFrames);
}


class GOLStates(midY: Int, midX: Int) {

	val i= midY;
	val j= midX;

	def queenBee(gen: GameOfLifeGeneration) {

		gen.ressurect(i - 1, j - 2);
		gen.ressurect(i - 2, j - 1);
		gen.ressurect(i - 3, j);
		gen.ressurect(i - 2, j + 1);
		gen.ressurect(i - 1, j + 2);

		gen.ressurect(i, j - 1);
		gen.ressurect(i, j);
		gen.ressurect(i, j + 1);

		gen.ressurect(i + 1, j - 3);
		gen.ressurect(i + 1, j - 2);
		gen.ressurect(i + 1, j + 2);
		gen.ressurect(i + 1, j + 3);
	}

	def lineMagic(gen: GameOfLifeGeneration) {

		val size= 5;

		for(m <- -size to size) {
			gen.ressurect(i + m, j - m);
		}

		gen.ressurect(i + size, j - (size + 1));
		gen.ressurect(i + size, j - (size + 2));
		gen.ressurect(i + size, j - (size + 3));
		gen.ressurect(i + size, j - (size + 4));
		gen.ressurect(i + size, j - (size + 5));

		gen.ressurect(i + (size + 1), j - size);
		gen.ressurect(i + (size + 2), j - size);
		gen.ressurect(i + (size + 3), j - size);
		gen.ressurect(i + (size + 4), j - size);
		gen.ressurect(i + (size + 5), j - size);

		gen.ressurect(i - size, j + (size + 1));
		gen.ressurect(i - (size - 1), j + (size + 1));
	}


	def birdey(gen: GameOfLifeGeneration) {

		gen.ressurect(i, j - 1);
		gen.ressurect(i, j);
		gen.ressurect(i, j + 1);
		gen.ressurect(i, j + 2);

		gen.ressurect(i + 1, j - 2);
		gen.ressurect(i + 3, j - 2);

		gen.ressurect(i + 1, j + 2);
		gen.ressurect(i + 2, j + 2);
		gen.ressurect(i + 3, j + 1);
	}
}



/**
 * Game of life generator
 * 
 * @param framerate: Int  The number of frames to render in one second
 */
class GameOfLife(framerate: Int, rows: Int, cols: Int) {

	// Terminal magic to clear the screen
	private val CLEAR_SCREEN_CODE= "\u001b[H\u001b[2J";

	// The generations
	private var _thisGen: GameOfLifeGeneration= _;
	private var _prevGen: GameOfLifeGeneration= _;

	// The initial state setting procedure
	private var _initialState: GameOfLifeGeneration => Unit= _; 

	// The frame left before terminating
	private var _totalNumberOfFrames= 0;


	/**
	 * Halts the current thread for some time
	 * 
	 * @param time  Sleep duration in milliseconds
	 */
	def sleep(time: Int) = Thread.sleep(time);

	/**
	 * Clear the terminal
	 */
	def clearScreen() = println(CLEAR_SCREEN_CODE);

	/**
	 * Skips a frame duration before executing the callback
	 * 
	 * @param  callback  Callback to run after skipping a frame
	 */
	def requestNextFrame(callback: ()=> Unit) {

		// 16 second sleep for 60fps
		sleep((1000/framerate).toInt);

		callback();
	}

	/**
	 * Setter for the initial game state
	 */
	def initialState_=(stateSetter: GameOfLifeGeneration => Unit) {

		this._initialState= stateSetter;

		this._setup();
	}

	/**
	 * Initializes the grid and renders the first frame
	 */
	private def _setup() {

		this._prevGen= new GameOfLifeGeneration(rows, cols);
		
		this._initialState(this._prevGen);

		this._prevGen.renderGrid();
	}


	/**
	 * Starts the render and calculation loops
	 * 
	 * @param frames  The number of frames to render
	 */
	def start(frames: Int = -1) {

		// Set the total number of frames left before 
		// the program terminates
		this._totalNumberOfFrames= frames;

		// Asynchronous calculation loop
		Future {
			requestNextFrame(
				this.calculationLoop(rows, cols)
			);
		}

		// Need to skip a frame to start the render loop to 
		// make sure the initial calculations are complete
		requestNextFrame(
			() => requestNextFrame(
				this.runRenderLoop()
			)
		);
	}

	/**
	 * Render loop - Renders the board on every frame
	 */
	def runRenderLoop()() {

		this.clearScreen();

		// Render the stuff to stdout
		_thisGen.renderGrid();

		// Decremenent the frames left
		if(this._totalNumberOfFrames > 0) {
			this._totalNumberOfFrames -= 1;
		}

		if(this._totalNumberOfFrames != 0) {
			requestNextFrame(
				this.runRenderLoop()
			);
		} else {
			System.exit(0);
		}
	}


	/**
	 * Calculation loop - Calculates the next generation on every frame
	 */
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

	/**
	 * Renders the current generation grid to stdout
	 */
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

	/**
	 * Set the cell state to ALIVE
	 *
	 * @param x  Cell position in the column
	 * @param y  Cell position in the row
	 */
	def ressurect(x: Int, y: Int) = {
		this.genGrid(x)(y)= this.ALIVE_STATE;
	}


	/**
	 * Get the number of cells alive around the current cell
	 *
	 * @param indexI The y-coordinate(row) of the cell
	 * @param indexJ The x-coordinate(column) of the cell
	 *
	 * @return       Number of cells alive
	 */
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

	/**
	 * Game of life rules
	 *
	 * @param live  The number of live cells in the neighborhood
	 * @param gen   The current generation
	 * @param row   Cell position in the row
	 * @param col   Cell position in the column
	 */
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

	/**
	 * Inherit cell properties from the previous generation
	 * 
	 * @param  prevGen   The previous generation
	 */
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


