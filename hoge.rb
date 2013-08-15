# http://homepage1.nifty.com/Ike/usapyon/HowToMakeShogiProgram.html

module KomaInfo
  OUT_OF_BOARD = 64
  EMPTY = 0
  FU = 1
  KY = 2
  KE = 3
  GI = 4
  KI = 5
  KA = 6
  HI = 7
  OU = 8
  PROMOTED = 8
  TO = PROMOTED + FU
  NY = PROMOTED + KY
  NK = PROMOTED + KE
  NG = PROMOTED + GI
  UM = PROMOTED + KA
  RY = PROMOTED + HI
  ENEMY = 16
  EFU = ENEMY + FU
  EKY = ENEMY + KY
  EKE = ENEMY + KE
  EGI = ENEMY + GI
  EKI = ENEMY + KI
  EKA = ENEMY + KA
  EHI = ENEMY + HI
  EOU = ENEMY + OU
  ETO = ENEMY + TO
  ENY = ENEMY + NY
  ENK = ENEMY + NK
  ENG = ENEMY + NG
  EUM = ENEMY + UM
  ERY = ENEMY + RY
  
  KOMA_STR = [
    "   "," 歩"," 香"," 桂"," 銀"," 金"," 角"," 飛"," 玉"," と"," 杏"," 圭"," 全"," 金"," 馬"," 竜",
    "   ","v歩","v香","v桂","v銀","v金","v角","v飛","v王","vと","v杏","v圭","v全","v金","v馬","v竜"]
  
  module_function
  def info_to_s(k)
    KOMA_STR[k]
  end
  
  def enemy?(k, sengo=false)
    if sengo
      self?(k)
    else
      k & ENEMY != 0
    end
  end

  def self?(k, sengo=false)
    if sengo
      enemy?(k)
    else
      FU <= k && k <= RY
    end
  end
end

class Kyokumen
  include KomaInfo

  def initialize
    board = Array.new(11) { Array.new(11, 0) }
    hand = Array.new(2) { Array.new(HI + 1, 0) }
  end
  
  def draw
    (FU..HI).each_with_index do |koma, i|
      text_wide info_to_s(koma), i * 60
    end
  end
end

FONT = StarRuby::Font.new("migmix-1p-regular.ttf", 50)

def text_wide(str, x=0, y=0, c=Color(0, 0, 0), s=StarRuby::Game.current.screen)
  s.render_text(str.toutf8, x, y, FONT, c, true)
end

kyokumen = Kyokumen.new

main(600, 600) {
  kyokumen.draw
}
