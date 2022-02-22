package com.mx.regexmaker

import scala.collection.mutable.HashMap


class TrieNode() {
  private var item: Char = 0
  private var repNumLow = 1 //repeat num lowerbound

  private var repNumUpp = 1 //repeat num upperbound

  private var itemLevel = 0 //item level

  private var inNum = 0 //number of items into this node

  private var endNum = 0 //number of items end from this node

  private var childNodes = new HashMap[Char, TrieNode]()


  def getItem: Char = item

  def setItem(item: Char): this.type = {
    this.item = item
    this
  }

  def getRepNumLow: Int = repNumLow

  def setRepNumLow(l: Int): this.type = {
    repNumLow = l
    this
  }

  def getRepNumUpp: Int = repNumUpp

  def setRepNumUpp(u: Int): this.type = {
    repNumUpp = u
    this
  }

  def getItemLevel: Int = itemLevel

  def setItemLevel(l: Int): this.type = {
    itemLevel = l
    this
  }

  def incItemLevel(): this.type = {
    itemLevel += 1
    this
  }

  def getInNum: Int = inNum

  def setInNum(n: Int): this.type = {
    inNum = n
    this
  }

  def incInNum(n: Int): this.type = {
    inNum += n
    this
  }

  def decInNum(n: Int): this.type = {
    inNum -= n
    this
  }

  def getEndNum: Int = endNum

  def incEndNum(n: Int): this.type = {
    endNum += n
    this
  }

  def decEndNum(n: Int): this.type = {
    endNum -= n
    this
  }

  def setEndNum(n: Int): this.type = {
    endNum = n
    this
  }

  def hasChilds = !childNodes.isEmpty

  def getChild(item: Char): Option[TrieNode] = childNodes.get(item)

  def getChildNodes = childNodes

  def setChildNodes(childs: HashMap[Char, TrieNode]): this.type = {
    childNodes.clear()
    childNodes ++= childs
    this
  }

  def clearChildNodes(): this.type = {
    childNodes.clear()
    this
  }

  // return child node
  def addChild(ch: Char, support: Int): TrieNode = {
    val child = childNodes.get(ch) match {
      case Some(child) =>
        child
      case None =>
        val child = TrieNode(ch)
        childNodes += ch -> child
        child

    }
    child.incInNum(support)
    child
  }


  def copy(n: TrieNode): this.type = {
    this.item = n.item
    this.repNumLow = n.repNumLow
    this.repNumUpp = n.repNumUpp
    this.inNum = n.inNum
    this.endNum = n.endNum
    this.itemLevel = n.itemLevel
    this.childNodes.clear()
    this.childNodes ++= n.childNodes
    this
  }


  def incRepNumUpp(supp: Int): this.type = {
    this.repNumUpp += supp
    this
  }

  def incRepNum(n: TrieNode): this.type = {
    this.repNumUpp += n.repNumUpp
    this.repNumLow += n.repNumLow
    this
  }

  def mergeRepNum(n: TrieNode): this.type = {
    if (this.repNumLow > n.repNumLow) this.repNumLow = n.repNumLow
    if (this.repNumUpp < n.repNumUpp) this.repNumUpp = n.repNumUpp
    this
  }

  /**
    * merge 2 nodes to one
    *
    * @param other
    */
  def mergeNode(other: TrieNode): this.type = {
    this.incInNum(other.getInNum)
      .incEndNum(other.getEndNum)
      .mergeRepNum(other)
      .mergeChildNodes(other)

    this
  }

  /**
    * merge the childs of node to current one
    *
    * @param other
    */
  def mergeChildNodes(other: TrieNode): this.type = {
    for ((_, otherChild) <- other.getChildNodes) {
      this.childNodes.get(otherChild.getItem) match {
        case Some(child) => child.mergeNode(otherChild)
        case None => this.childNodes += otherChild.getItem -> otherChild
      }
    }

    this
  }

}

object TrieNode {
  def apply(): TrieNode = new TrieNode()

  def apply(item: Char): TrieNode = {
    val t = new TrieNode()
    t.item = item
    t
  }

  def apply(n: TrieNode): TrieNode = new TrieNode().copy(n)
}
