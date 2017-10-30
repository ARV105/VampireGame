/**
 * Created by Auriel on 1/20/2015.
 Create puddles of water to hinder the vampire from reaching you.
 Remember vampires cannot cross over bodies of water well. vampires dont die when puddles given,
 timer needs to be fixed.
 */
 
import java.awt.{Graphics2D,Color}
import scala.swing._
import event._
import java.awt.geom._
import java.awt.image.BufferedImage
import javax.swing.Timer

case class Enemy(x:Int, y:Int,timer:Int)
case class Player(x:Int,y:Int)

val PanelSize = 600

var enemies = List(Enemy(PanelSize-100,PanelSize-100,0))
var player = Player(300,300)
var leftPressed = false
var rightPressed = false
var upPressed = false
var downPressed = false
var currentTime = 0
var regenDelay = 100
var enemyLifespan= 100
val puddleImage = new
    BufferedImage(PanelSize, PanelSize, BufferedImage.TYPE_INT_ARGB)
for(i <- 0 until PanelSize; j <- 0 until PanelSize) {
  puddleImage.setRGB(i,j,0)
}


val panel=new Panel {
  override def paint(g:Graphics2D): Unit = {
    //Draw to g
    g.setPaint(Color.black)
    g.fillRect(0, 0, size.width, size.height)
    g.drawImage(puddleImage, 0, 0, null)
    for (enemy <- enemies) {
      g.setPaint(new Color(1f, 0f, 0f,
        (currentTime - enemy.timer) / enemyLifespan.toFloat))
      g.fill(new Ellipse2D.Double(enemy.x - 5, enemy.y - 5, 10, 10))
      val dx = enemy.x - player.x
      val dy = enemy.y - player.y
      if (dx * dx + dy * dy < 25) {
        g.setPaint(Color.white)
        g.drawString("You Lose!", 200, 200)
        timer.stop
      }
    }
    g.setPaint(Color.green)
    g.fill(new Ellipse2D.Double(player.x - 5, player.y - 5, 10, 10))
  }

  val imageG= puddleImage.createGraphics()
  imageG.setColor(new Color(200,150,50))

  def drawPuddle(x:Int, y:Int): Unit = {
  val radX = util.Random.nextInt(5)+5
    val radY = util.Random.nextInt(5)+5
    imageG.fill(new Ellipse2D.Double(x-radX, y-radY, radX*2, radY*2))
    repaint
  }

  listenTo(mouse.clicks,mouse.moves,keys)
  reactions += {
    case mp:MousePressed => drawPuddle(mp.point.x, mp.point.y)
    case md:MousePressed => drawPuddle(md.point.x, md.point.y)
    case me:MouseEntered => requestFocus
    case kp:KeyPressed =>
     if(kp.key ==Key.Left) leftPressed = true
      if(kp.key ==Key.Right) rightPressed = true
      if(kp.key ==Key.Up) upPressed = true
      if(kp.key ==Key.Down) downPressed = true
    case kp:KeyReleased =>
      if(kp.key ==Key.Left) leftPressed = false
      if(kp.key ==Key.Right) rightPressed = false
      if(kp.key ==Key.Up) upPressed = false
      if(kp.key ==Key.Down) downPressed = false
  }
  preferredSize = new Dimension(PanelSize,PanelSize)
}

  def inMud(x:Int, y:Int):Boolean = {
    (puddleImage.getRGB(x,y) >>> 24) > 0
  }

  val timer: Timer = new Timer(100,Swing.ActionListener(e => {
    val speed = if(inMud(player.x,player.y)) 1 else 3
    if(leftPressed) player = player.copy(x = player.x-speed)
    if(rightPressed) player = player.copy(x = player.x+speed)
    if(upPressed) player = player.copy(y = player.y-speed)
    if(downPressed) player = player.copy(y = player.y+speed)
    enemies = for(e <- enemies; if e.timer+enemyLifespan>currentTime) yield {
      val speed = if(inMud(e.x,e.y)) 1 else 4
      val dx = if(e.x<player.x) speed else if(e.x>player.x) -speed else 0
      val dy = if(e.y<player.y) speed else if(e.y>player.y) -speed else 0
      e.copy(x = e.x+dx+util.Random.nextInt(5)-2,
        y = e.y+dy+util.Random.nextInt(5)-2)
    }
    for(i <- 0 until PanelSize; j <- 0 until PanelSize) {
      val argb = puddleImage.getRGB(i,j)
      val alpha = argb >>> 24
      if(alpha>0) puddleImage.setRGB(i,j,(argb & 0xffffff) | ((alpha-1) << 24))
    }
    currentTime += 1
    if(currentTime%regenDelay==0) {
      val cx = util.Random.nextInt(2)
      val cy = util.Random.nextInt(PanelSize-10)
      enemies ::= Enemy(10+cx*(PanelSize-20), cy, currentTime)
      if(regenDelay>10) regenDelay -= 1
      if(math.random<0.1) enemyLifespan += 1
    }
    panel.repaint()
  }))
val frame = new MainFrame {
  title = "Puddle Dash"
  contents = panel
  menuBar = new MenuBar {
      contents += new Menu("File") {
      contents += new MenuItem(Action("Start")(timer.start))
      contents += new Separator
      contents += new MenuItem(Action("Exit")(sys.exit(0)))
    }
  }
}
frame.visible = true
panel.requestFocus
