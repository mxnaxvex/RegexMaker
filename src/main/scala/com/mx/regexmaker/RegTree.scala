package com.mx.regexmaker

import scala.collection.mutable
import scala.collection.mutable.HashMap

class RegTree {
  private val CHAR_ALL: Char = 0 // level 3
  private val CHAR_WORD: Char = 1 //DIGITAL LETTER _   level 2
  private val LETTER: Char = 2 //a-z A-Z \c level 1
  private val LETTER_U: Char = 3 //A-Z \L level 1
  private val LETTER_L: Char = 4 //a-z \l level 1
  private val DIGITAL: Char = 5 //0 -9  \d ,level 1
  private val OTHERLANG: Char = 6 // other language  level 1
  private val BLANK: Char = 7 // \s   level 1

  private val SPECIALCHAR = BLANK

  private var maxLevelUp = 3 //字符最大升级级别，可自定义

  private var curLevel = 1
  private val rootId: Char = 65535


  /**
    * 剪枝比例，分支的数据数量与所有姊妹分支平均数的比例 小于此比例是认为是脏数据
    */
  private var pruneScore = 0.3
  private var pruneDirtyData = true

  /**
    * 若某个分支的数据量占比大于此值，则不对该分支进行升级，如此可保留较大比重数据的一些细节信息
    *   1.0 表示关闭此功能
    */
  private var absGoodBranch = 1.0

  private var absGoodBranchNum = 0 // total num * absGoodBranch


  val ROOT = TrieNode(rootId).setItemLevel(99) // the source tree


  /**
    * 控制节点只有一个子节点，且值相等时是否进行深度方向的合并
    * 通过深度合并，可产生\d{3}这样的结果
    */
  private var mergeDepth = false
  /**
    * 统计树的不同分支长度包含的数据量，用于深度方向的合并
    */
  private var lengthInfo = new HashMap[Int, Int]

  /**
    * 分支数阈值，分支数超过阈值时对分支进行升级并合并
    */
  private val Branch_Num = 3

  val LEVELUPTABLE: Array[Char] = Array[Char]( //level 0 up to level 1
    BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK,
    BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK,
    BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK,
    33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    DIGITAL, DIGITAL, DIGITAL, DIGITAL, DIGITAL, DIGITAL, DIGITAL, DIGITAL, DIGITAL, DIGITAL,
    58, 59, 60, 61, 62, 63, 64, LETTER_U, LETTER_U, LETTER_U, LETTER_U, LETTER_U,
    LETTER_U, LETTER_U, LETTER_U, LETTER_U, LETTER_U, LETTER_U, LETTER_U, LETTER_U,
    LETTER_U, LETTER_U, LETTER_U, LETTER_U, LETTER_U, LETTER_U, LETTER_U, LETTER_U,
    LETTER_U, LETTER_U, LETTER_U, LETTER_U, LETTER_U, 91, 92, 93, 94, 95, 96,
    LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L,
    LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L,
    LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L, LETTER_L,
    LETTER_L, LETTER_L, 123, 124, 125, 126, 127)

  val RegMap = Map[Char, String](DIGITAL -> "\\d", LETTER_L -> "[a-z]",
    LETTER_U -> "[A-Z]", LETTER -> "[a-zA-z]", OTHERLANG -> "[\\u0100-\\uffff]",
    BLANK -> "\\s", CHAR_WORD -> "\\w", CHAR_ALL -> ".", rootId -> "") ++
    ".\\?*+|{}()[]".toCharArray.map { c => c -> ("\\" + c.toString) }.toMap

  /**
    * 将字符c从itemLevel升级到curLevel
    */
  private def charLevelUp(c: Char, itemLevel: Int): Char = {
    if (itemLevel >= curLevel) return c
    val upC =
      itemLevel match {
        case 0 => if (c > 127) OTHERLANG else LEVELUPTABLE(c)
        case 1 => c match {
          case DIGITAL => CHAR_WORD
          case LETTER_L => CHAR_WORD
          case LETTER_U => CHAR_WORD
          case '_' => CHAR_WORD
          case _ => c
        }
        case _ => CHAR_ALL
      }
    if (itemLevel < curLevel - 1) charLevelUp(upC, itemLevel + 1)
    else upC
  }

  /**
    * 往树中插入一条数据
    *
    * @param data
    * @param support 数据重复的次数
    * @return
    */
  def addData(data: String, support: Int = 1): this.type = {
    if (data == null || data.isEmpty) return this
    val len = data.length
    lengthInfo.get(len) match {
      case Some(n) => lengthInfo += len -> (n + support)
      case None => lengthInfo += len -> support
    }
    var curNode = ROOT
    curNode.incInNum(support)
    for (ch <- data) {
      curNode = curNode.addChild(ch, support)
    }
    curNode.incEndNum(support)
    this
  }

  def addData(data: Iterable[String]): this.type = {
    data.foreach(d => addData(d, 1))
    this
  }


  /**
    * 存在多个子节点时，与父节点值相同时是否向父节点合并
    * 经验判断规则，思路：大量数据分布在少量几个分支长度时，非必要下先不进行深度方向的合并，节点的升级时可能将相关子树合并在一起，
    * 另外可以考虑：如果子节点往父节点合并后，后面的子树只有一个子分支，则可以合并；
    */
  private def checkLengthInfo(): Unit = {
    mergeDepth = false
    if (lengthInfo.isEmpty) return
    val maxGroups = (1 / (absGoodBranch + 0.000001)).toInt + 1
    val avg = ROOT.getInNum / lengthInfo.size / 10
    var groups = 0
    lengthInfo.values.foreach(supp =>
      if (supp >= avg) groups += 1
    )
    if (groups > maxGroups) mergeDepth = true
  }


  def mine: Array[String] = {
    checkLengthInfo()
    absGoodBranchNum = (ROOT.getInNum * absGoodBranch).toInt
    //记录迭代过程中的中间结果
    val regsMap = new mutable.HashMap[String, Int]()
    curLevel = 1
    var changedNum = 0
    do {
      changedNum = levelUp(ROOT)
      changedNum += mergeDepth(ROOT)
      if (changedNum > 0) {
        val reg = getRegString()
        regsMap.get(reg) match {
          case Some(n) => regsMap.put(reg, n + 1)
          case None => regsMap.put(reg, 1)
        }
      }
      curLevel += 1
    } while (curLevel <= maxLevelUp && changedNum > 0)

    regsMap.toArray.sortBy(x => (-x._2, x._1.length)).map(_._1)
  }

  /**
    * 将node的所有子节点升到当前级curLevel，并对相同的节点进行合并
    *
    * @param node
    * @return 所有已升级的节点数
    */
  private def levelUp(node: TrieNode): Int = {
    levelUp(node, false)
  }

  /**
    * 将node的所有子节点升到当前级curLevel，并对相同的节点进行合并
    *
    * @param node
    * @param forceLu 是否强制升级。若为否，则依据分支数来判断是否需要升级
    * @return 所有已升级的节点数
    */
  private def levelUp(node: TrieNode, forceLu: Boolean): Int = {
    var lvNum = 0
    val absGoodNum = (node.getInNum * absGoodBranch).toInt
    val newChilds = new HashMap[Char, TrieNode]()
    // 除去好的分支，剩下的分支数较少时也不升级
    val forceLu2 = {
      var num = 0
      node.getChildNodes.values.foreach(n => if (n.getInNum < absGoodNum) num += 1)
      num >= Branch_Num
    }

    for (child <- node.getChildNodes.values) {
      if (forceLu || (child.getInNum < absGoodNum && forceLu2)) {
        //子节点需要升级
        lvNum += 1
        val itemUp = charLevelUp(child.getItem, child.getItemLevel)
        child.setItemLevel(curLevel)
        child.setItem(itemUp)
        lvNum += levelUp(child, true)
        newChilds.get(itemUp) match {
          case Some(ns) =>
            ns.mergeNode(child)
          case None =>
            newChilds += itemUp -> child
        }
      }
      else {
        lvNum += levelUp(child, false)
        newChilds += child.getItem -> child
      }
    }
    node.setChildNodes(newChilds)
    prune(node)
    lvNum
  }

  /**
    * 深度方向的合并，子节点往父节点合并
    *
    * @param node
    * @return 所有合并的节点数
    */
  private def mergeDepth(node: TrieNode): Int = {
    var mNum = 0
    if (!node.hasChilds) return 0
    val childs = node.getChildNodes
    childs.values.foreach(c => mNum += mergeDepth(c))
    prune(node)
    childs.get(node.getItem) match {
      case Some(child) =>
        //存在与父节点相同item的子节点
        if (childs.size == 1 && node.getEndNum == 0) {
          //只有1个相同的子节点，跟父节点合并
          mNum += 1
          node.setChildNodes(child.getChildNodes)
          node.incRepNum(child)
          node.setInNum(child.getInNum)
          node.setEndNum(child.getEndNum)
        } else if (mergeDepth) {
          mNum += 1
          childs.remove(child.getItem)
          node.mergeChildNodes(child)
          node.incRepNumUpp(child.getRepNumUpp)
          node.incEndNum(child.getEndNum)
          prune(node)
        }
      case None =>
    }

    mNum
  }

  /**
    * 剪枝，分支的数据数量与所有姊妹分支平均数的比例小于阈值的分支从树中剪去
    *
    * @param node
    */
  private def prune(node: TrieNode): Unit = {
    if (!pruneDirtyData || !node.hasChilds) return
    val childs = node.getChildNodes
    val size = if (node.getEndNum > 0) childs.size + 1 else childs.size

    val minSize = (node.getInNum / size * pruneScore).toInt

    if (node.getEndNum < minSize) node.setEndNum(0)
    val pruneChilds = new mutable.HashSet[Char]
    childs.foreach(c => if (c._2.getInNum < minSize) pruneChilds += c._1)
    pruneChilds.foreach(c => childs.remove(c))

  }

  /**
    * 获得正则表达式
    *
    * @return
    */
  def getRegString(): String = convertTree2String(ROOT, true)

  private def convertTree2String(node: TrieNode, bHead: Boolean): String = {
    if (!node.hasChilds) return ""
    //获得所有子分支的规则表达式
    val childRegs = node.getChildNodes.values.map(convert1Node _)
    if (childRegs.size == 1) return childRegs.head
    //存在多个分支，则用逻辑或组装
    if (bHead) childRegs.mkString("|") else childRegs.mkString("(?:", "|", ")")
  }

  private def convert1Node(node: TrieNode): String = {
    val item = node.getItem
    //节点的值转换成正则表达式中的字符串
    val itemReg = getItemChar(item)
    val reg = new mutable.StringBuilder(itemReg)
    //处理节点重复次数
    val rl = node.getRepNumLow
    val ru = node.getRepNumUpp
    if (rl == ru) {
      if (rl > 1) {
        if (rl < 4 && item < 128 && item > SPECIALCHAR) reg.append(itemReg * (rl - 1))
        else reg.append("{").append(rl).append("}")
      }
    } else reg.append("{").append(rl).append(",").append(ru).append("}")
    if (!node.hasChilds) return reg.toString()
    //获取子树的正则表达式
    val childReg = convertTree2String(node, false)
    if (childReg.isEmpty) return reg.toString()
    //终止数不为零，说明子树的规则是可选的，因此要加问号
    if (node.getEndNum > 0) reg.append("(?:").append(childReg).append(")?")
    else reg.append(childReg)

    reg.toString()
  }

  private def getItemChar(c: Char): String = {
    RegMap.get(c) match {
      case Some(r) => r
      case None => c.toString
    }
  }


}

object RegTree {
  def main(args: Array[String]): Unit = {
    Test1

    Test2

  }

  def Test1(): Unit = {
    val data = "国药准字Z22020031\n国药准字Z44021605\n国药准字H20052134\n国药准字H20065128\n" +
      "国药准字H20063321\n国药准字Z45020047\n国药准字Z44023696\n国药准字Z44020041\n" +
      "国药准字B20020729\n国药准字Z37021536\n国药准字Z10970003\n国药准字Z20025893\n" +
      "国药准字Z20025021\n国药准字Z41021818\n国药准字Z13021046\n国药准字Z10980075\n" +
      "国药准字Z22020445\n国药准字Z45021256\n国药准字Z20027946\n国药准字Z20027945\n" +
      "国药准字Z20054513\n国药准字Z20064282\n国药准字Z20064281\n国药准字Z20063129\n" +
      "国药准字Z20053531"
    val reg = new RegTree
    reg.addData(data.split("\n"))
    // 加两条脏数据
    reg.addData("J20030052")
      .addData("国药准字")
    reg.mine
    println(reg.getRegString())
  }

  def Test2(): Unit = {
    val reg = new RegTree
    //3个字母+3个数字 系列
    reg.addData("abc123", 5)
      .addData("cdd334", 3)
      .addData("fdk324", 8)
      .addData("odv082", 1)
      .addData("jdv492", 3)
      .addData("mfd729", 5)
      .addData("uepw158", 1)
      .addData("ids491", 1)
    //4个字母+3个数字 系列
    reg.addData("kkkk653", 20)
    //11位数字 系列
    reg.addData("13567891234", 20)
      .addData("13812345678", 20)
    //脏数据
    reg.addData("ovd4321", 1)
      .addData("123456789012345")

    val r = reg.mine.mkString("\n\n")
    println(r)
  }

}
