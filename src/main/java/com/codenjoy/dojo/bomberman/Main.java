package com.codenjoy.dojo.bomberman;

import com.codenjoy.dojo.client.Solver;
import com.codenjoy.dojo.client.WebSocketRunner;

public class Main implements Solver<Board> {

  private static final String USER_NAME = "bratiakina@gmail.com";

  private BSolver aiSolver;

  public Main() {
    this.aiSolver = new BSolver();
  }

  @Override
  public String get(Board board) {
    if (board.isGameOver()) return "";
    return aiSolver.solve(board);
  }

  public static void main(String[] args) {
    start(USER_NAME, WebSocketRunner.Host.REMOTE);
  }

  public static void start(String name, WebSocketRunner.Host server) {
    try {
      WebSocketRunner.run(server, name,
        new Main(),
        new Board());
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

}
