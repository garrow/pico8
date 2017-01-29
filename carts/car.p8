pico-8 cartridge // http://www.pico-8.com
version 8
__lua__
-- car jelpi test
-- by zep

-- to do:
-- levels and monsters
-- title / restart logic
-- block loot
-- top-solid ground
-- better duping

-- review: want slopes? complex.
-- use if cute implemntation
 -- slopey ground deflection
 -- slopey ground slowdown

car_x=0
num_players = 1
corrupt_mode = false
paint_mode = false
show_debug_info = false
max_actors = 128
level=0

--music(0, 0, 3)


function make_actor(k,x,y,d)
	local a = {}
	a.kind = k
	a.life = 1
	a.x=x a.y=y a.dx=0 a.dy=0
	a.ddy = 0.06 -- gravity
 a.w=0.3 a.h=0.5 -- half-width
 a.d=d a.bounce=0.8
 a.frame = 1  a.f0 = 0
 a.t=0
 a.standing = false
 if (count(actor) < max_actors) then
  add(actor, a)
 end
	return a
end

function make_sparkle(x,y,frame,col)
 local s = {}
 s.x=x
 s.y=y
 s.frame=frame
 s.col=col
 s.t=0 s.max_t = 8+rnd(4)
 s.dx = 0 s.dy = 0
 s.ddy = 0
 add(sparkle,s)
 return s
end

function make_player(x, y, d)
 pl = make_actor(1, x, y, d)
 pl.charge = 0
 pl.super  = 0
 pl.score  = 0
 pl.bounce = 0
 pl.delay  = 0
 pl.id     = 0 -- player 1
 pl.pal    = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
 
 return pl
end

function init_level(lev)

 level=lev

 -- copy section of map
 memcpy(0x2000,
   0x1000+((lev+2)%4)*0x800,
   0x800)
   

 -- spawn player
 for y=0,15 do for x=0,127 do
  if (mget(x,y) == 48) then
   player = make_player(x+0.5,y+0.5,1)

   if (num_players==2) then
    player2 = make_player(x+2,y+1,1)
    player2.id = 1
    player2.pal = {1,3,3,4,5,6,7,11,9,10,11,12,13,15,7} 
   end
   
  end
 end end

end

-- called at start by pico-8
function _init()

 actor = {}
 sparkle = {}
 
 init_level(level)
 
 
 t = 0
 
 death_t = 0
end

-- clear_cel using neighbour val
-- prefer empty, then non-ground
-- then left neighbour
function clear_cel(x, y)
 val0 = mget(x-1,y)
 val1 = mget(x+1,y)
 if (val0 == 0 or val1 == 0) then
  mset(x,y,0)
 elseif (not fget(val1,1)) then
  mset(x,y,val1)
 else
  mset(x,y,val0)
 end
end


function move_spawns(x0, y0)

 x0=flr(x0)
 y0=flr(y0)
 -- spawn stuff close to x0,y0

 for y=0,16 do
  for x=x0-10,x0+10 do
   val = mget(x,y)
   m = nil

   -- pickup
   if (fget(val, 5)) then    
    m = make_actor(2,x+0.5,y+0,1)
    m.f0 = val
    m.frame = val
    if (fget(val,4)) then
     m.ddy = 0 -- zero gravity
    end
   end

   -- monster
   if (fget(val, 3)) then
    m = make_actor(3,x+0.5,y+1,-1)
    m.f0=val
    m.frame=val
   end
   
   -- clear cel if spawned something
   if (m ~= nil) then
    clear_cel(x,y)
   end
  end
 end

end
-- test if a point is 

function solid (x, y)
	if (x < 0 or x >= 128 ) then
		return true end
				
	val = mget(x, y)
	return fget(val, 1)
end

function move_pickup(a)
 a.frame = a.f0
-- if (flr((t/4) % 2) == 0) then
--  a.frame = a.f0+1
-- end
end

function move_player(pl)

 local b = pl.id

 if (pl.life == 0) then
    death_t = 1
    for i=1,32 do
     s=make_sparkle(
      pl.x, pl.y-0.6, 96, 0)
     s.dx = cos(i/32)/2
     s.dy = sin(i/32)/2
     s.max_t = 30 
     s.ddy = 0.01
     s.frame=96+rnd(3)
     s.col = 7
    end
    
    del(actor,pl)
    
    sfx(16)
    music(-1)
    sfx(5)

  return
 end


 accel = 0.05
 if (pl.charge > 10) then
  accel = 0.08
 end
 
 if (not pl.standing) then
  accel = accel / 2
 end
  
 -- player control
	if (btn(2,b)) pl.dy-=0.1
	if (btn(0,b)) then 
			pl.dx = pl.dx - accel; pl.d=-1 end
	if (btn(1,b)) then 
		pl.dx = pl.dx + accel; pl.d=1 end

	if ((btn(4,b)) and 
--		solid(pl.x,pl.y)) then 
  pl.standing) then
		pl.dy = -0.7
  sfx(8)
 end

 -- charge

 if (btn(5,b) and pl.charge == 0
 and pl.delay == 0) then
  pl.charge = 15
  
  -- charge in dir of buttons
  dx=0 dy=0
  if (btn(0,b)) dx-=1
  if (btn(1,b)) dx+=1
  if (btn(2,b)) dy-=0.6
  if (btn(3,b)) dy+=1
  if (dx==0 and dy==0) then
   pl.dx = pl.dx + pl.d * 0.4
  else
   pl.dx += dx *0.5
   pl.dy += dy *0.5
  end
  
  if (not pl.standing) then
   pl.dy = pl.dy - 0.2
  end 
 
  sfx(11)
 
 end
 
 -- charging
 
	if (pl.charge > 0 or
	    pl.super  > 0) then
	 pl.frame = 53
	 
	 if (abs(pl.dx) > 0.4 or
	     abs(pl.dy) > 0.2
	 ) then
	 
	 for i=1,3 do
	  local s = make_sparkle(
	   pl.x+pl.dx*i/3, 
	   pl.y+pl.dy*i/3 - 0.3,
	   96+rnd(3), (pl.t*3+i)%9+7)
	  if (rnd(2) < 1) then
	   s.col = 7
	  end
	  s.dx = -pl.dx*0.1
	  s.dy = -0.05*i/4
	  s.x = s.x + rnd(0.6)-0.3
	  s.y = s.y + rnd(0.6)-0.3
  end
  end
	end 
	
	pl.charge = max(0, pl.charge-1)
 if (pl.charge > 0) then
  pl.delay = 10
 else
  pl.delay = max(0,pl.delay-1)
 end

 pl.super = max(0, pl.super-1)
 
 -- frame	

 if (pl.standing) then
	 pl.f0 = (pl.f0+abs(pl.dx)*2+4) % 4
 else
	 pl.f0 = (pl.f0+abs(pl.dx)/2+4) % 4 
 end
 
 if (abs(pl.dx) < 0.1) 
 then
  pl.frame=48 -- pl.f0=0
 else
	 pl.frame = 49+flr(pl.f0)
	end

 if (pl != player2) then
  pl.frame = pl.frame +75-48
 end
	
end

function move_monster(m)
 m.dx = m.dx + m.d * 0.02

	m.f0 = (m.f0+abs(m.dx)*3+4) % 4
 m.frame = 112 + flr(m.f0)

 if (false and m.standing and rnd(100) < 1)
 then
  m.dy = -1
 end

end

function move_actor(pl)

 -- to do: replace with callbacks

 if (pl.kind == 1) then
  move_player(pl)
 end
 
 if (pl.kind == 2) then
  move_pickup(pl)
 end

 if (pl.kind == 3) then
  move_monster(pl)
 end

 pl.standing=false
 
 
 -- x movement 
	
 x1 = pl.x + pl.dx +
      sgn(pl.dx) * 0.2
      
 local broke_block = false

 if(not solid(x1,pl.y-0.5,1)) then
		pl.x = pl.x + pl.dx  
	else -- hit wall
		
	 -- search for contact point
	 while (not solid(pl.x + sgn(pl.dx)*0.3, pl.y-0.5)) do
	  pl.x = pl.x + sgn(pl.dx) * 0.1
	 end

  -- if charging, break block	
	 if (pl.charge ~= nil) then
	 
   if (pl.charge > 0 or 
       pl.super  > 0) then
    val = mget(x1, pl.y-0.5,0)
    
    if (fget(val,4)) then
     clear_cel(x1,pl.y-0.5)
     sfx(10)
     broke_block = true
     
     -- make debris
     
     for by=0,1 do
      for bx=0,1 do
       s=make_sparkle(
       0.25+flr(x1) + bx*0.5, 
       0.25+flr(pl.y-0.5) + by*0.5,
       22, 0)
       s.dx = (bx-0.5)/4
       s.dy = (by-0.5)/4
       s.max_t = 30 
       s.ddy = 0.02
      end
     end
     
    else
     if (abs(pl.dx) > 0.2) then
      sfx(12) -- thump
     end
    end
    
    -- bumping kills charge
    if (pl.charge < 20) then
     pl.charge = 0
    end
    
   end
	 end

  -- bounce	
  if (pl.super == 0 or 
      not broke_block) then
   pl.dx = pl.dx * -0.5
  end

  if (pl.kind == 3) then
   pl.d = pl.d * -1
   pl.dx=0
  end

	end
	
 -- y movement

 if (pl.dy < 0) then
  -- going up
  
  if (solid(pl.x-0.2, pl.y+pl.dy-1) or
   solid(pl.x+0.2, pl.y+pl.dy-1))
  then
   pl.dy=0
   
   -- search up for collision point
   while ( not (
   solid(pl.x-0.2, pl.y-1) or
   solid(pl.x+0.2, pl.y-1)))
   do
    pl.y = pl.y - 0.01
   end

  else
   pl.y = pl.y + pl.dy
  end

	else

  -- going down
 
  if (solid(pl.x-0.2, pl.y+pl.dy) or
   solid(pl.x+0.2, pl.y+pl.dy)) then

	  -- bounce
   if (pl.bounce > 0 and 
       pl.dy > 0.2) 
   then
    pl.dy = pl.dy * -pl.bounce
   else
 
    pl.standing=true
    pl.dy = 0
    
   end
    
   

   --snap down
   while (not (
     solid(pl.x-0.2,pl.y) or
     solid(pl.x+0.2,pl.y)
     ))
    do pl.y = pl.y + 0.05 end
     
  else
   pl.y = pl.y + pl.dy  
  end

  -- pop up
  
  while 
  solid(pl.x+0.2,pl.y-0.05,128+1) or
  solid(pl.x-0.2,pl.y-0.05,128+1) 
  do
   pl.y -= 0.05
   pl.dx *=0.9
   --transfer horizontal speed to vertical
   pl.dy = min(pl.dy, 
   -abs(pl.dx)*2)
   
   --pl.dx /=2
   ----pl.dx=0
   --pl.dy=-0.5
  end

 end

   

 -- gravity and friction
	pl.dy = pl.dy + pl.ddy
 pl.dy = pl.dy * 0.95

 -- x friction
 if (pl.standing) then
 	pl.dx = pl.dx * 0.8
	else
 	pl.dx = pl.dx * 0.9
	end

 -- counters
 pl.t = pl.t + 1
end

function collide_event(a1, a2)
 if(a1.kind==1) then
  if(a2.kind==2) then

   if (a2.frame==64) then
    a1.super = 120
    a1.dx = a1.dx * 2
    --a1.dy = a1.dy-0.1
   -- a1.standing = false
    sfx(13)
   end

   -- gem
   if (a2.frame==80) then
    a1.score = a1.score + 1
    sfx(9)
   end

   del(actor,a2)

  end
  
  -- charge or dupe monster
  
  if(a2.kind==3) then -- monster
   if(a1.charge > 0 or 
      a1.super  > 0 or
     (a1.y-a1.dy) < a2.y-0.7) then
    -- slow down player
    a1.dx = a1.dx * 0.7
    a1.dy = a1.dy * -0.7-- - 0.2
    
    -- explode
    for i=1,16 do
     s=make_sparkle(
      a2.x, a2.y-0.5, 96+rnd(3), 7)
     s.dx = s.dx + rnd(0.4)-0.2
     s.dy = s.dy + rnd(0.4)-0.2
     s.max_t = 30 
     s.ddy = 0.01
     
    end
    
    -- kill monster
    -- to do: in move_monster
    sfx(14)
    del(actor,a2)
    
   else

    -- player death
    a1.life=0


   end
  end
   
 end
end

function move_sparkle(sp)
 if (sp.t > sp.max_t) then
  del(sparkle,sp)
 end
 
 sp.x = sp.x + sp.dx
 sp.y = sp.y + sp.dy
 sp.dy= sp.dy+ sp.ddy
 sp.t = sp.t + 1
end


function collide(a1, a2)
 if (a1==a2) then return end
 local dx = a1.x - a2.x
 local dy = a1.y - a2.y
 if (abs(dx) < a1.w+a2.w) then
  if (abs(dy) < a1.h+a2.h) then
   collide_event(a1, a2)
  end
 end
end

function collisions()

 for a1 in all(actor) do
  collide(player,a1)
 end

 if (player2 ~= nil) then
  for a1 in all(actor) do
   collide(player2,a1)
  end
 end

end

function outgame_logic()

 if (death_t > 0) then
  death_t = death_t + 1
  if (death_t > 30 and 
   btn(4) or btn(5))
  then 
    music(-1)
    sfx(-1)
    sfx(0)
    dpal={0,1,1, 2,1,13,6,
          4,4,9,3, 13,1,13,14}
          
    -- palette fade
    for i=0,40 do
     for j=1,15 do
      col = j
      for k=1,((i+(j%5))/4) do
       col=dpal[col]
      end
      pal(j,col,1)
     end
     flip()
    end
    
    -- restart cart end of slice
    run()
   end
 end
end

function _update()

	foreach(actor, move_actor)		
	foreach(sparkle, move_sparkle)
 collisions()
 move_spawns(player.x, player.y)

 outgame_logic()
 
 if (corrupt_mode) then
  for i=1,5 do
   poke(rnd(0x8000),rnd(0x100))
  end
 end
 
	t=t+1
end

function draw_sparkle(s)
 
 if (s.col > 0) then
  for i=1,15 do
   pal(i,s.col)
  end
 end

 spr(s.frame, s.x*8-4, s.y*8-4)

 pal()
end

function draw_actor(pl)

 if (pl.pal ~= nil) then
  for i=1,15 do
--   pal(i, pl.pal[i])
  end
 end

 if (pl.charge ~= nil and 
     pl.charge > 0) then
 
  for i=2,15 do
   pal(i,7+((pl.t/2) % 8))
  end
--  pal(2,7)

 end

 if (pl.super ~= nil and 
     pl.super > 0) then
 
  for i=2,15 do
   pal(i,6+((pl.t/2) % 2))
  end

 end
  
	spr(pl.frame, 
  pl.x*8-4, pl.y*8-8, 
  1, 1, pl.d < 0)
  
 pal()
end


function apply_paint()
 if (tt==nil) tt=0
 tt=tt+0.25
 srand(flr(tt))
 local nn=rnd(128)
 local xx=0
 local yy=band(nn,127)
 for i=1,1000*13,13 do
  nn+=i
  nn*=33
  xx=band(nn,127)
  local col=pget(xx,yy)
  rectfill(xx,yy,xx+1,yy+1,col)
  line(xx-1,yy-1,xx+2,yy+2,col)
  nn+=i
  nn*=57
  yy=band(nn,127)
  rectfill(xx-1,yy-1,xx,yy,pget(xx,yy))
   
 end
end

function _draw()

 -- sky
	camera (0, 0)
	--rectfill (0,0,127,127-32,12) 
	
 -- sky gradient
 if (true) then
 for y=0,127 do
  col=sget(level,
  mid(1,5,(y+(y%4)*6) / 16))
  line(0,y,127,y,col)
 end
 end

 --stars
 --if (sget(level,0)==7)
 if (false)
 then
 mapdraw (32, 59, 0, 0, 16, 16, 0)
 end
 
 -- background bottom colour 
 rectfill (0,128-32,127,127,
  sget(level,7)) 

 -- clouds behind mountains
 if (sget(level,0) > 0) then
  pal(7,sget(level,0))
  local x = t / 8
  x = x % 128
  mapdraw(16, 59, -x, 56, 16, 16, 0)
  mapdraw(16, 59, 128-x, 56, 16, 16, 0)
 end
 -- mountains
 
 if (sget(level,6)>0) then
 
  local bgcol = sget(level,7)
  pal(5,bgcol) pal(2,bgcol)
  pal(13,sget(level,6)) 
  mapdraw (0, 59, 0, 56, 16, 16, 0)
 	pal()
 end
 
 -- map and actors
	cam_x = mid(0,player.x*8-64,1024-128)

 if (player2 ~= nil) then
  cam_x = 
   mid(0,player2.x*8-64,1024-128) / 2 +
   cam_x / 2
 end
 
 --cam_y = mid(0,player.y*6-40,128)
 cam_y = 0
	camera (cam_x,cam_y)
 pal(12,0)	
	mapdraw (0,0,0,0,128,64,1)
 pal()
 foreach(sparkle, draw_sparkle)
	foreach(actor, draw_actor)
	
--	spr(72, car_x,88, 2, 2)
	car_x += 0.5
	if (rnd(100)<1) sfx(20)
	
 -- forground map
--	mapdraw (0,0,0,0,128,64,2)

 -- player score
 camera(0,0)
 color(7)
 
 if (death_t > 60) then
  print("press button to restart",
   18-1,10-0,8+(t/4)%2)
  print("press button to restart",
   18,10,7)
 end
 
 if (paint_mode) apply_paint()

 if (show_debug_info) then
  pal()
  cursor(0,2)
  color(7)
  print("actors:"..count(actor))
  print("score:"..player.score)
  print(stat(1))
 end
 
end








__gfx__
71700000000000004444444433b333b30000000000000000effffff7d66667d666666667d6666667cccccccccccccccc2000000025522552cc5ccccc20000000
c11000000000000044444444333333330000000000eeee002effff7f5d66765d666666765d666676ccccccccccccccc55200000052255225c55555cc50000000
c1200000007007004424444422222222000000000eeee7e022eeeeff55dd6655dddddd6655dddd66cccccccccccccc55252000002552255255555ccc20000000
c180000000077000444444444444444400000000eeeeeeee22eeeeff55dd6655dddddd6655dddd66ccccccccccccc5555252000052255225c555cccc52020000
c190000000077000444444444444444400070000eeeeeeee22eeeeff55dd6655dddddd6655dddd66cccccccccccccc5c2525200025522552cc5ccccc25252000
c1a00000007007004444424444444244007a70002222222222eeeeff55dd6655dddddd6655dddd66ccccccccccc55555525252005225522555cccccc52525000
6d000000000000004444444444444444000700000002d000221111ef5111d651111111d6511111d6cccccccccc55c55525252520255225525ccccccc25252500
d0c0000000000000444444444444444400030000000de0002111111e11111d111111111d1111111dccccccccc55555555252525252255225cccccccc52525250
00067000555ddd661010122244440404000b00000e0aa000000000000000000000000000cc5cccc5000000000000000ddddd2525dddddddd000000002525252d
0006700055dd666711101222444404440b3b000000d99090000000000000000000000000c555555500000077000000dd5ddd5252dddddddd000600005252525d
00566700c5d6667c0100112444442400003b0000a9777d0000eff7000000000000000000555555550000077700000dd525d5dd252ddddd2500676000252525dd
00566700c5d6667c0111122244442400000b0000a97779a0002eef000000000000333300c5555555000077770000ddd252ddddd252dddd520006000052525ddd
05d66670cc5667cc0000122244440000000b3b000d7779a0002eef000000000003333330cc5ccc5c00777777000dddd5252ddd252525d5250000000025252ddd
05d66670cc5667cc0000112444444000000b30009099d00000211e0000000000333a33335555c5550777777700dddd52525d52525252525200000000525252dd
55dd6667ccc67ccc0001222224444400000b000000aa0e00000000000000000033a7a33355555555077777770dd5d525252525252525252500000000252525dd
555ddd66ccc67ccc0011111224444440000b0000000300000000000000000000333a33335555c55577777777dd5dd2525252525252525252000000005252525d
444244444444444444444444424242424242424200000000333333333333333333333333bbbbbbbb000000000000000ddddd2525dddddddd0000000000000000
444444242444244444444444222422442424242400000000333333333333b33333333333bbbbbbbb77000000000000dddddd5252dddddddd0000000000000000
4242424242424424444244444242422242424442000000003339a3333333bab3333333333333333377770000000000ddddd52525dddddddd0000000000000000
242424242422244444444424242424242424244400707000339a7a33333bbb333333333333333333777770000000ddddddd25252dddddddd0000000000000000
4442224242424244424442444244224242124242000e00003399a93333333b33333333333bb33b3377777700000dddddddd52525dddddddd0000060000000000
422424242424242444442424222444242422222400737000333993333b333333333333333bb333337777770000ddd5dddd525252dddddddd0000000000000000
4442424242424444424442424222424242421242000b000033333333333333333333333333333333777777700ddd5dddd5252525dddddddd00000000000d0000
2424242424242424242424242424242424242424000b00003333333333333333333333333333333377777777d5ddddddd2525252dddddddd0000000000000000
00000000000000000f000f000f000f00000000000000000000bbbbbbbbbbbb003333333300333300777777770000000000000000000000000000000aa0000000
0f000f000f000f000ffffff00ffffff00f000f00000000000bbbbbbbbbbbbbb0333333330333333077777777000000000000000000000000000000a33a000000
0ffffff00ffffff00f1fff100f1fff100ffffff000000000bb333333333333bb33333333333333337777777700000000000000000000000000000ab33ba00000
0f1fff100f1fff100effffe00effffe00f1fff1000077000b33333333333333b3333333333333b33777777770000000000000000000000000000abb33bba0000
0efffff00effffe000222000002220000effffe0000770003333bb3333333333331331333333333377777777000000000000000000000000000abbb33bbba000
002220000022200000888f000f88800000222000000000003333bb3333bb3333313113133b3333337777777700000000000000000000000000abbbb33bbbba00
0088800000888f000f00000000000f000f888000000000003333333333bb33331311113133333333777777770000000000000000000000000abbbbb33bbbbba0
00f0f00000f0000000000000000000000000f000000000003333333333333333111111113333333377777777000000000000000000000000a33333333333333a
00777700006666000077770000777700e7e7e7e7e7e7e7e707777770066666600000000000000000000000000000000000000000000000000000000000000000
07000070060000600700007007000070222222222222222270000007600000060000000000000000000000000000000000000000000000000888888008888880
700077076000770670099007700990072ff22fff2fff2f2270007707600077060000000000000000000000000888888008888880088888800867887008678870
70b077076000770670999907709999072f222f2f2f2f2f2270007707600077060000000000000000000000000867887008678870086788700866886008668860
70ae000760c0000679999997799999972f2f2f2f2fff2f2270000007600000060000888888800000000000000866886008668860086688608888888888888888
707c8007607e000670099007700990072fff2fff2f2f2ff270000007600000060000888888880000000000008888888888888888888888888888888888888888
07000070060000600709907007099070222222222222222270000007600000060008888888880000000000008558855885588558855885580550055005500550
00777700006666000077770000777700e7e7e7e7e7e7e7e707777770066666600008777887770000000000000550055005500550055005500550055005500550
0000000000000000000bb00000000000000000000000000000000000000000000008777887770000000000000000000000000000000000000000000000000000
00000000000ef0000000b00000000000000000000000000000000000000000000008777887770000000000000000000000000000000000000000000000000000
be82887700ed7f000994399000000000be8288770000000000000000000000008888888888888888000000000000000000000000000000000000000000000000
be8828eb0edef7f099a9979900000000be8828eb0000000000000000000000008888888888888888000000000000000000000000000000000000000000000000
bfe88efb0f7fede0949999a9aaa00000bfe88efb0000000000000000000000008885588888885588000000000000000000000000000000000000000000000000
0bfeefb000f7de0094499999a0aaaaa00bfeefb00000000000000000000000008855558888855558000000000000000000000000000000000000000000000000
00bbbb00000fe00009449990a0a0a0a000bbbb000000000000000000000000000055550000055550000000000000000000000000000000000000000000000000
000000000000000000999900aaa000a0000000000000000000000000000000000005500000005500000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000008888888800000000000000008888888888888888000000000000000088888888
00000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000880000000000000000000000000000008
00000000000700000000000000000000000000000000000000000000000000008000000000000000000000000000000880000000000000000000000000000008
00077000007070000000000000000000000000000000000000000000000000008000000000000000000000000000000880000000000000000000000000000008
00077000000700000000700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000077770000777700076767670076667000076700006767600000000000000000000000000000000000000000000000000000000000000000
0077770000777700077787700777e770000050000000500000005000000050000000000000000000000000000000000000000000000000000000000000000000
077787700777b7700777777007777770007777000077770000777700007777000000000000000000000000000000000000000000000000000000000000000000
07777770077777700717771007177710077787700777877007778770077787700000000000000000000000000000000000000000000000000000000000000000
07177710071777100777777007777770071777100717771007177710071777108000000000000000000000000000000880000000000000000000000000000008
0777777007777770a999999009a99990077777700777777007777770077777708000000000000000000000000000000880000000000000000000000000000008
0999999a0999999000000a000000000a099999900999999009999990099999908000000000000000000000000000000880000000000000000000000000000008
00a000000a0000a0000000000000000000a00a0000a000a00a0000a00a000a008888888800000000000000008888888888888888000000000000000088888888
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00050000000000006060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00050000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00050000000000000000606060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000606060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00050000007474740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00640000007474740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
64646464646464646464646464646464646464646464646464809070809000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
82828282900000000000000000000000828282829000000000000000000000008282828290000000000000000000000082828282900000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
82000082909000000000000000005200820000829090000000000000000052008200008290900000000000000000520082000082909000000000000000005200
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
82000082909090009090909030303030820000829090900090909090303030308200008290909000909090903030303082000082909090009090909030303030
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
82828282909090909090909020202020828282829090909090909090202020208282828290909090909090902020202082828282909090909090909020202020
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1c0c000000000000000000000000000000000000000000000000000c0c0c0c1c0000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1c0000000000000000000000000000000000000000000000000c0c000000000c0000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1c001c001c00000000000000000000000000000000000000000000000000000c0000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2c2c2c1c1c0000000000000000000000000000000000000c0c000c000c0c0c1c0000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000b1f00000000000000000000000a1a200000000000000000000e100e200000000e10000f2000000260000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000b1c00000b2c2d0f0b1f00000000000a1a2a1a3a3a2000000000000000000e20000000000f2000000000000e100e200000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c0b2c2d0f0b2d2c2d0d0c1d0f00000b1a1a3a3a3a3a3a3a2a1a20000a1a2000000e10000e2000000000000f20000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
d0d2c2d0d0d1d1d0d0d0d0d0d0c0b2c2a3a3a3a3a3a3a3a3a3a3a2a1a3a3a2a10000f20000000000e2000000000000f200000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
d0d1d0d0d0d0d0d0d0d0d0d0d0d0d1d0a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a300000000000000000000000000f2000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__label__
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc6dccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccc7777cccccccccccccccccccccccccccc66dccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccc7777777ccccccccccccccccccccccccc66ddccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccc777777777ccccccccccccccccccccccc666dddcdcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccc777777777777ccccccccccccccccccccc6666ddddddccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccc7777777777777cccccccccccccccccccc6666dddddddccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccc77777777777777cccccccccccccccccc66d6dddddddddcccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccc7777777777777777cccccccccccccccc66d66ddddddddddccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccc6d777777777777777ccccccccccccccc66666dddddddddddddcccccccccccccc6dccccccccccccccccccccccccccccccccccccccc
cccccc7777cccccccccccc66dd7777777777777777cccccccccccc666666dddddddddddddccccccccccccc66dccccccccccccccccccccccccccccccccccccccc
ccccc7777777ccccccccc66dddd77777777777777777cccccccccc66666ddddddddddddddcccccccccccc66ddccccccccccccccccccccccccccccccccccccccc
cccc777777777ccccccc666ddddd77777777777777777ccccccc6666666dddddddddddddddcdcccccccc666dddcdcccccccccccccccccccccccccccccccccccc
cc777777777777cccc76666dddddd77777777777777777ccccc66666666ddddddddddddddddddcccccc6666ddddddccccccccccccccccccccccccccccccccccc
c7777777777777ccc76666dddddddd7777777777777777cccc666d6666dddddddddddddddddddccccc6666dddddddccccccccccccccccccccccccccccccccccc
c77777777777777cc66d6dddddddddd7777777777777777cc666d6666dddddddddddddddddddddccc66d6dddddddddcccccccccccccccccccccccccccccccccc
777777777777777766d66ddddddddddd77777777777777776d6666666ddddddddddddddddddddddc66d66ddddddddddccccccccccccccccccccccccccccccccc
d7777777777777766666ddddddddddddd777777777777776666666666666dddddddddddddddddddd6666dddddddddddddcccccccccccccccccccccccccccccc6
dd777777777777666666ddddddddddddd777777777777766666666666666ddddddddddddddddddddd666ddddddddddddd7cccccccccccccccccccccccccccc66
ddd7777777777766666dddddddddddddd77777777777776666666666666ddddddddddddddddddddddd6d66ddddddddddd777ccccccccccccccccccccccccc66d
dddd777777776666666ddddddddddddddd7d77777777666666666666666ddddddddddddddddddddddd66666ddddddddddd7d7ccccccccccccccccccccccc666d
ddddd77777766666666dddddddddddddddddd7777776666666666666666dddddddddddddddddddddddd666ddddddddddddddd7cccccccccccccccccccc76666d
dddddd7777666d6666ddddddddddddddddddd77777666d666666666666ddddddddddddddddddddddddd6ddddddddddddddddd7ccccccccccccccccccc76666dd
ddddddd77666d6666ddddddddddddddddddddd777666d666666666666ddddddddddddddddddddddddddddddddddddddddddddd7cccccccccccccccccc66d6ddd
dddddddd6d6666666dddddddddddddddddddddd76d666666666666666dddddddddddddddddddddddddddddddddddddddddddddd7cccccccccccccccc66d66ddd
dddddddd6e6aa6666666dddddddddddddddddddd6666666666666666dddddddddddddddddddddddddddddddddddddddddddddddddcccccccccccccc66666dddd
dddddddd66d996966666dddddddddddddddddddd6666666666666666ddddddddddddddddddddddddddddddddddddddddddddddddddcccccccccccc666666dddd
dddddddda9777d66666dddddddddddddddddddddd66666ddd66666ddddddddddddddddddddddddddddddddddddddddddddddddddddd7ccccccccc766666ddddd
dddddddda97779a6666ddddddddddddddddddddddd6666dddd6666dddddddddddddddddddddddddddddddddddddddddddddddddddddd7ccccccc6666666ddddd
dddddddd6d7779a6666ddddddddddddddddddddddddd6ddddddd6dddddddddddddddddddddddddddddddddddddddddddddddddddddddd7cccc766666666ddddd
dddddddd9699d66666ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddccc7666d6666dddddd
dddddddd66aa6e666ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddcc666d6666ddddddd
dddddddd666366666ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd6d6666666ddddddd
dddddddd666b6666dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd66666666dddddddd
dddddddd6b3b6666dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd66666666dddddddd
ddddddddd63b66ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd66666dddddddddd
dd7d7ddddd6b66dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd6666dddddddddd
dddedddddddb3bdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd6ddddddddddd
dd737ddddddb3ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddbdddddddbdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddbdddddddbdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddbdddddddbddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddeffffff7dddddddddddddddddddddddd
db3bdddddb3bdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd2effff7fdddddddddddddddddddddddd
dd3bdddddd3bddddddddddddddddddddd888888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddd22eeeeffdddddddddddddddddddddddd
dddbdddddddbddddddddddddddddddddd867887ddddddddddddddddddddddddddddddddddddddddddddddddddddddddd22eeeeffdddddddddddddddddddddddd
dddb3bdddddb3bddddd7ddddddddddddd866886ddddddddddddddddddddddddddddddddddddddddddddddddddddddddd22eeeeffdddddddddddddddddddddddd
dddb3ddddddb3ddddd7a7ddddddddddd88888888dddddddddddddddddddddddddddddddddddddddddddddddddddddddd22eeeeffdddddddddddddddddddddddd
dddbdddddddbddddddd7dddddddddddd85588558dddddddddddddddddddddddddddddddddddddddddddddddddddddddd221111efdddddddddddddddddddddddd
dddbdddddddbddddddd3ddddddddddddd55dd55ddddddddddddddddddddddddddddddddddddddddddddddddddddddddd2111111edddddddddddddddddddddddd
33b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b333b3
33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444244444442444444424444444244444442444444424444444244444442444444424444444244444442444444424444444244444442444444424444444244
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44424444444244444424444444244444442444444424444444244444442444444424444444244444442444444424444444244444442444444424444444424444
44444424444444244444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444424
42444244424442444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444442444244
44442424444424244444424444444244444442444444424444444244444442444444424444444244444442444444424444444244444442444444424444442424
42444242424442424444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444442444242
24242424242424244444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444424242424
42424242424242424444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444442444442424242
22242244222422442444244444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444442422242244
42424222424242224242442444424444444244444442444444424444444244444442444444424444444244444442444444424444444244444242424242424222
24242424242424242422244444444424444444244444442444444424444444244444442444444424444444244444442444444424444444242424242424242424
42442242424422424242424442444244424442444244424442444244424442444244424442444244424442444244424442444244424442444442224242442242
22244424222444242424242444442424444424244444242444442424444424244444242444442424444424244444242444442424444424244224242422244424
42224242422242424242444442444242424442424244424242444242424442424244424242444242424442424244424242444242424442424442424242224242
24242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424

__gff__
0000030301811303030301010000010003030101010103030101000000000000030303030301010101030100000000000000000000000303010101000000818120002000010103030000000000000000200020202000000000000000000000000000000000000000000000000000000008000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000500050005000500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000505050505040000000000009000900090009000900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000004b4c00000000000000000000000000000000000000000000000000000000000009090708090909090900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000005b5c00000000000000000000000000000000000000000000000000000000000009090a0a0a0a0a070800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000003629293700000000000000000000000015000000000000090a530a0a0a0a0a0b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000015000000
00000000000000000000000000000000000000000000000000000036292929293700000000002728262700000000000000000000000014250000000000090a0a0a0a520a0b1900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000014150000
00000000000000000000000000000000000000500000000000000028272827262700000000002728272800000000000000000303030303030000000000090a0a0a700a0b190900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000014140000
0000000000000000000000000000000000000000000000000000002726282827280000000000282728270000000000003918020222222202391800000009070809070809070800000000000000000000000000000000000000000000000000000000000000001500000000000000000000000000000000000000000044450000
0000000000000000000000000000000000000606060000000000002828272827260000000000383838380039183918392828222023232421282839391803030303030303030303030015000000000000000000000000000000000000000000000000000000001400000000000000000000000000000000000000000014140000
0000000000000000000000000000000000000606060000000000002628282828270000000070001213003938060638382828232324232323282728282820202320202020202020203914000000000000000000000000000000000000000000001500000000001415000000000000000000000000000000000603030303030303
0015000000000000000000000000000000000000000000000000003838383838380000000003030303030303030303032828282828282827282728282823232323232323232323232739000300000000000000000000000606000000000015001415000000151414000000000000000000000000000000030302020202020202
2514000000000000000000000000000000001500000000000000000000121300000070000002020202020222220202022828282828282728272828282828282828282828282828272828180203030303030300000070000606000000150014001414000000145014000000000000700000000000700000020202020202020202
1414040030000000000000000600000025001400000000007000030303030303030303030302022222222023232102023838383838383838383838383838380606060606060638383838380202020202020203030303030303007000140414001414000404141414000000000303030303030303030303020202020202020202
0303030303030303030303030303030303030303030303030303020202020222020202022202202323232323232321020303030303030303030303030303030303030303030303700370030202020202020202020202020202030303030303030303030303030303030303030202020202020202020202020202020202020202
2222020202020202020202020202022222222222020202020202020202202323212222202320232323232323232323212202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202
2323212222222222222222222222202323232323212222020202022220232323232323232323232323232323232323232321220202020222222202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000003e020202020202020202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000003e02000200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000003e0202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
250000000000000000000000000000003e020200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1400000000000000000000000000003e02020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
14000000000600000000000000003e0202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
140025000006060000000000003e020202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1415140000000000000000003e030202023f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
14141425000000000000003e0202020202023f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
141414140000003000003e02020202020202023f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0303030303030303030303020202020202020203030303030303030300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0202020202020202020202020202020202020202020202020202020200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0202020202020202020202020202020202020202020202020202020200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0202020202020202020202020202020202020202020202020202020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
01030000185701c5701f57024570185701c5701f57024560185601c5601f56024560185501c5501f550245501a5501d5501f540245401a5301d5301f530235301a5301d5301f5301a5201d510215102451023515
01100000240452400528000280452b0450c005280450000529042240162d04500005307553c5252d000130052b0451f006260352b026260420c0052404500005230450c00521045230461f0450c0051c0421c025
01100000187451a7001c7001c7451d745187001c7451f7001a745247001d7451d70021745277002470023745217451f7001d7001d7451a7451b7001c7451f7001a745227001c7451b70018745187001f7451f700
01100000305453c52500600006003e625006000c30318600355250050000600006003e625006000060018600295263251529515006003e625006000060018600305250050018601006003e625246040060000600
01100000004750c47518475004750a475004750a4750c475004750a4750c475004750a4750c4751147513475004750c4750a475004750a475004750a4750c475004750c47516475004751647518475114750c475
01100000180721a0751b0721f0721e0751f0751e0721f075270752607724075200721f0751b0771a0751b07518072180621805218042180350000000000000000000000000000000000000000000000000000000
011000000c37518375243751f3751b3721a372193711b372183721837217371163511533114311133001830214302143021830218302003000030000300003000030000300003000030000300003000030000300
011000000c37300300003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300
000000001e0701f070220702a020340103f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000100002b7602e7503a73033740377302e75033730337303372035710377103a710337103a7103c7103c7003f700007000070000700007000070000700007000070000700007000070000700007000070000700
00020000236501d650126501b65012640126301362017630176300b6301361012610116100d6101c6101260013600106000d60010600116000e6001160012600116000a600066000960003600026000260002600
000100002257524575275652455527555275552b54524525225352252527525275252b5252e515305152e515305052e505305052e5053050530505335052b5052e5052b5052e5052e5053350530505335052e505
000200002005325043160231002304013030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0102000013571165731b5751d5711157313575165711b5731b575225711b573185751b5711f573245751b5711f57324565295611f563185611d555245532b5552b5412b5433053137535335333a5212b5252e513
000200002b071270711b07118071100710b0710607104071040610606103061040510305101041010310102101011040110000000000000000000000000000000000000000000000000000000000000000000000
010200002e17029170171731a171231631d16111143141610c1230a11107110001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100
01040000185702257024570225701f5701d5701f5701d57018570165701857016570135701157013570115700c5700d570135701457018560195501f550205302453024520225202452022510245102251024500
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000c00001f4701f4101f4701f47000400004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
01 01434144
00 02434144
00 01034244
02 02034244
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144
00 41414144

