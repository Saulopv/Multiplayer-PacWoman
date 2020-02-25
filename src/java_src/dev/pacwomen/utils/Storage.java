package dev.pacwomen.utils;

import java.util.HashMap;

public class Storage {
  private HashMap<Tuple,Block> storage;
  private static Storage singleton = new Storage();

  private Storage() {
    this.storage = new HashMap<>();
  }

  public static Storage getInstance() {
    return singleton;
  }

  int getSize() {
    return this.storage.size();
  }

  void put(Tuple tuple, Block block) {
    this.storage.put(tuple,block);
  }

  Block getBlock(Tuple key) {
    return this.storage.get(key);
  }

  void clearStorage() {
    this.storage.clear();
  }
}
