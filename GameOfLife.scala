#!/usr/bin/env scala

import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class GameOfLifeGeneration(rows: Int, cols: Int) {

	val ALIVE_STATE = '#';
	val DEAD_STATE  = '-';

	val genGrid= Array.fill[Char](rows, cols) { DEAD_STATE };

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

	def setFirstGeneration() {

		// val random= new Random();

		this.genGrid(rows/2)(cols/2)= ALIVE_STATE;
		this.genGrid(rows/2 - 1)(cols/2 - 1)= ALIVE_STATE;
		this.genGrid(rows/2 - 1)(cols/2)= ALIVE_STATE;
		this.genGrid(rows/2 - 1)(cols/2 + 1)= ALIVE_STATE;
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
					
					if(m >= rows) {
						m %= rows;
					} else if(m < 0) {
						m = rows + m;
					}

					if(n >= cols) {
						n %= cols;
					} else if(n < 0) {
						n = cols + n;
					}

					if(this.genGrid(m)(n) == ALIVE_STATE) {
						numOfLiveCells += 1;
					}
				}
			}
		}

		return numOfLiveCells;
	}

	def inheritFrom(prevGen: GameOfLifeGeneration) {

		for(i <- 0 until rows) {

			for(j <- 0 until cols) {

				var numOfLiveCells= prevGen.getNeighbourLiveCount(i, j);

				if(prevGen.genGrid(i)(j) == ALIVE_STATE) {

					if(numOfLiveCells == 3) {
						this.genGrid(i)(j)= ALIVE_STATE;
					}

				} else {

					if(numOfLiveCells == 2 || numOfLiveCells == 3) {
						this.genGrid(i)(j)= ALIVE_STATE;
					}
				}
			}
		}
	}
}


class GameOfLife(framerate: Int) {

	val CLEAR_SCREEN_CODE= "\u001b[H\u001b[2J";


	var thisGen: GameOfLifeGeneration= _;
	var prevGen: GameOfLifeGeneration= _;

	var frameReadyToRender: Boolean= true;


	// Put the current thread to sleep for `time`ms
	def sleep(time: Int) = Thread.sleep(time);

	// Clears the terminal screen
	def clearScreen() = println(CLEAR_SCREEN_CODE);

	def requestNextFrame(callback: ()=> Unit) {

		// 16 second sleep for 60fps
		sleep((1000/framerate).toInt);

		callback();
	}

	def start(row: Int, col: Int) {

		prevGen= new GameOfLifeGeneration(row, col);
		prevGen.setFirstGeneration();

		// Asynchronous render loop
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
		thisGen.renderGrid();

		requestNextFrame(
			this.runRenderLoop()
		);
	}

	def calculationLoop(row: Int, col: Int)() {

		thisGen= new GameOfLifeGeneration(row, col);

		thisGen.inheritFrom(prevGen);

		prevGen= thisGen;

		requestNextFrame(
			this.calculationLoop(row, col)
		);
	}
}


val game= new GameOfLife(3);

game.start(50, 50);