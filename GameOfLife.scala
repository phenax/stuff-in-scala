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

	def inheritFrom(prevGen: GameOfLifeGeneration) {

		for(i <- 0 until rows) {

			for(j <- 0 until cols) {

				var numOfLiveCells= 0;

				for(m <- -1 to 1) {
					
					for(n <- -1 to 1) {

						var indexM= m;
						var indexN= n;

						if(i + indexM <= rows)
							indexM %= rows;

						if(j + indexN <= cols)
							indexN %= cols;						

						if(prevGen.genGrid(i + m)(j + n) == ALIVE_STATE) {
							numOfLiveCells += 1;
						}
					}
				}

				if(prevGen.genGrid(i)(j) == ALIVE_STATE) {

					numOfLiveCells -= 1;

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


object GameOfLife {

	val CLEAR_SCREEN_CODE= "\u001b[H\u001b[2J";


	var thisGen: GameOfLifeGeneration= _;
	var prevGen: GameOfLifeGeneration= _;

	var frameReadyToRender: Boolean= true;


	// Put the current thread to sleep for `time`ms
	def sleep(time: Int) = Thread.sleep(time);

	// Clears the terminal screen
	def clearScreen() = println(CLEAR_SCREEN_CODE);

	def init(row: Int, col: Int)() {

		prevGen= new GameOfLifeGeneration(row, col);
		prevGen.setFirstGeneration();

		// Asynchronous render loop
		val f = Future {
			while(true)
				this.calculationLoop(row, col);
		}

		sleep(16);

		// On the main process so that it doesnt exit out
		while(true)
			this.runRenderLoop()
	}

	def runRenderLoop() {

		// 16 second sleep for 60fps
		sleep(16);

		this.clearScreen();

		// Render the stuff to stdout if its ready to render
		if(this.frameReadyToRender)
			thisGen.renderGrid();
	}

	def calculationLoop(row: Int, col: Int) {

		thisGen= new GameOfLifeGeneration(row, col);
		
		this.frameReadyToRender= false;
		
		thisGen.inheritFrom(prevGen);

		this.frameReadyToRender= true;

		sleep(16);
	}
}

GameOfLife.init(10, 10)();