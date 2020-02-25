package dev.pacwomen.utils;

public class Block {
    private Tuple coordinates;
    private Translate.State state;
    private Translate.Collectable collectable;
    private Translate.Entity entity;

  public Block(Tuple coordinates, Translate.State state,
               Translate.Collectable collectable, Translate.Entity entity) {
    this.coordinates = coordinates;
    this.state = state;
    this.collectable = collectable;
    this.entity = entity;
  }

  public Tuple getCoordinates() {
    return coordinates;
  }

  public void setCoordinates(Tuple coordinates) {
    this.coordinates = coordinates;
  }

  public Translate.State getState() {
    return state;
  }

  public void setState(Translate.State state) {
    this.state = state;
  }

  public Translate.Collectable getCollectable() {
    return collectable;
  }

  public void setCollectable(Translate.Collectable collectable) {
    this.collectable = collectable;
  }

  public Translate.Entity getEntity() {
    return entity;
  }

  public void setEntity(Translate.Entity entity) {
    this.entity = entity;
  }

  @Override
  public String toString() {
    String coordinates = this.coordinates.toString();
    String state = this.state.toString();
    String collectable = this.collectable.toString();
    String entity = this.entity.toString();
    return "("+coordinates+",{"+state+","+collectable+","+entity+"})";
  }
}
