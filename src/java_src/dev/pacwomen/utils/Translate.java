package dev.pacwomen.utils;

import com.ericsson.otp.erlang.*;
import dev.pacwomen.worlds.World;

public class Translate {
  enum State {
      WALL, TILE
  }
  enum Collectable {
    EMPTY, FRUIT, POINT, POWERUP
  }
  enum Entity {
    EMPTY, NPC, PLAYER
  }
    
  /**
   * This function is used once at the start of the system to initialize
   * all the objects that is represented by the game-map.
   *
   * It does the necessary translation from OtpDataTypes to Java standard Object types
   * that is needed.
   * @param OtpMap is the whole game-map used to initialize all the blocks. Contains
   *               all the coordinates and what is on the coordinates of each tile/block.
   * @param OtpTuple gives the dimension of the map.
   * @throws OtpErlangRangeException e
   */
  public void translate(OtpErlangMap OtpMap,OtpErlangTuple OtpTuple) throws OtpErlangRangeException{
    Tuple<Integer,Integer> tuple = tupleConvert(OtpTuple);
    for(int i=0; i<tuple.getX(); i++) {
      for(int j=0; j<tuple.getY(); j++) {
        OtpErlangObject[] tmp = new OtpErlangObject[]{
               new OtpErlangLong(i),
               new OtpErlangLong(j)
        };
        Tuple<Integer,Integer> coordinatesTuple = new Tuple<>(i,j);
        OtpErlangTuple value = (OtpErlangTuple) OtpMap.get(new OtpErlangTuple(tmp));

        mapValues(value,coordinatesTuple);
      }
    }
  }

  /**
   * This function changes both the block that the entity is currently on and the
   * it was on before it moved.
   * It does the necessary translation from OtpDataTypes to Java standard Object types
   * that is needed.
   * @param newPosition is a tuple containing all the new information about the new state
   *                    of the block for the entity to move.
   * @param oldCoordination is the coordinates to the old block that the entity was in
   *                       before it moved to another block.
   * @throws an exception
   */
    void translate(OtpErlangTuple newPosition, OtpErlangTuple oldCoordination) throws OtpErlangRangeException{
    Storage storage = Storage.getInstance();
    Tuple newCoordination = tupleConvert((OtpErlangTuple) newPosition.elementAt(0));
    OtpErlangAtom entity = (OtpErlangAtom) newPosition.elementAt(1);
    Tuple oldCoordinations = tupleConvert(oldCoordination);

    if(entity.atomValue().equals("player")) {
      Block newBlock = storage.getBlock(newCoordination);
      newBlock.setEntity(Entity.PLAYER);
      Block oldBlock = storage.getBlock(oldCoordinations);
      oldBlock.setEntity(Entity.EMPTY);
      oldBlock.setCollectable(Collectable.EMPTY);
    }
    if(entity.atomValue().equals("npc")) {
      Block newBlock = storage.getBlock(newCoordination);
      newBlock.setEntity(Entity.NPC);
      Block oldBlock = storage.getBlock(oldCoordinations);
      oldBlock.setEntity(Entity.EMPTY);
    }
  }

  /**
   * Is used to take the output from the Erlang node as input thus only
   * for receiving a Otp map and the dimension or a tuple with the changes
   * of a block to another.
   * It also does the decision if it is a block that is needed to be translated
   * or a whole game-map.
   * It does the necessary translation from OtpDataTypes to Java Object types
   * that is needed.
   * @param obj the message being received.
   * @throws OtpErlangRangeException e
   */
  public void receive(OtpErlangObject obj) throws OtpErlangRangeException {
    if(obj instanceof OtpErlangTuple &&
            ((OtpErlangTuple) obj).elementAt(0) instanceof OtpErlangMap) {
      OtpErlangMap map = (OtpErlangMap) ((OtpErlangTuple) obj).elementAt(0);
      OtpErlangTuple dimension = (OtpErlangTuple) ((OtpErlangTuple) obj).elementAt(1);
      translate(map,dimension);
    }
    if(obj instanceof OtpErlangTuple &&
            ((OtpErlangTuple) obj).elementAt(0) instanceof OtpErlangTuple) {
      OtpErlangTuple newPosition = (OtpErlangTuple) ((OtpErlangTuple) obj).elementAt(0);
      OtpErlangTuple oldCoordination = (OtpErlangTuple) ((OtpErlangTuple) obj).elementAt(1);
      translate(newPosition,oldCoordination);
    }
  }

  /**
   * Initialize the objects and puts it in the hashMap.
   * It does the necessary translation from OtpDataTypes to Java Object types
   * that is needed.
   * @param OtpTuple contains all the necessary Enums to initialize the block.
   * @param tuple is the blocks key.
   */
  void mapValues(OtpErlangTuple OtpTuple,Tuple tuple) {
    Storage storage = Storage.getInstance();
    State state = translateState((OtpErlangAtom)OtpTuple.elementAt(0));
    Collectable collectable = translateCollectable((OtpErlangAtom)OtpTuple.elementAt(1));

    OtpErlangObject entityobject = OtpTuple.elementAt(2);
    Entity entity;
    if (entityobject instanceof OtpErlangAtom) {
	entity = translateEntity((OtpErlangAtom)OtpTuple.elementAt(2));}
    else
	{
	    entity = translateEntity((OtpErlangPid)OtpTuple.elementAt(2));
	}

    Block block = new Block(tuple,state,collectable,entity);
    storage.put(tuple,block);
  }

  /**
   * Is used to get the key that is represented as tuples to the hashMap that contains
   * all the objects to the map.
   * It does the necessary translation from OtpDataTypes to Java Object types
   * that is needed.
   * @param OtpTuple is the tuple that is going to be translated.
   * @throws OtpErlangRangeException
   * @return a tuple that can be used as a key to the hashMap.
   */
  Tuple<Integer,Integer> tupleConvert(OtpErlangTuple OtpTuple) throws OtpErlangRangeException{
    OtpErlangLong x = (OtpErlangLong) OtpTuple.elementAt(0);
    OtpErlangLong y = (OtpErlangLong) OtpTuple.elementAt(1);
    return new Tuple<>(x.intValue(),y.intValue());
  }

  /**
   * Takes a atom and translates it to the Enum representation.
   * It does the necessary translation from OtpDataTypes to Java Object types
   * that is needed.
   * @param atom is the entity that is going to be translated.
   * @return the Enum which represents the atom in the translated state.
   */
  State translateState(OtpErlangAtom atom) {
    String str = atom.atomValue();
    return State.valueOf(str.toUpperCase());
  }

  /**
   * Takes a atom and translates it to the Enum representation.
   * It does the necessary translation from OtpDataTypes to Java Object types
   * that is needed.
   * @param atom is the entity that is going to be translated.
   * @return the Enum which represents the atom in the translated state.
   */
  Collectable translateCollectable(OtpErlangAtom atom) {
    String str = atom.atomValue();
    return Collectable.valueOf(str.toUpperCase());
  }

  /**
   * Takes a atom and translates it to the Enum representation.
   * It does the necessary translation from OtpDataTypes to Java Object types
   * that is needed.
   * @param atom is the entity that is going to be translated.
   * @return the Enum which represents the atom in the translated state.
   */
  Entity translateEntity(OtpErlangAtom atom) {
    String str = atom.atomValue();
    return Entity.valueOf(str.toUpperCase());
  }

    Entity translateEntity(OtpErlangPid Pid) {
	return Entity.valueOf("NPC");
    }
}
