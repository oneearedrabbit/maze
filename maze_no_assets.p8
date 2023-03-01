pico-8 cartridge // http://www.pico-8.com
version 41
__lua__
-- [compression]
-- [compression]
function px9_sdecomp(
    x0,y0, -- where to draw to
    src,   -- compressed data
    vget,  -- read fn (x,y)
    vset   -- write fn (x,y,v)
)
    local function vlist_val(l, val)
        -- find position
        local v,i=l[1],1
        while v!=val do
            i+=1
            v,l[i]=l[i],v
        end
        l[1]=val
    end

    -- bit cache is between 16 and 
    -- 22 bits long with the next
    -- bit always aligned to the
    -- lsb of the fractional part
    local cache,cache_bits,i=0,0,1
    local function getval(bits)
        while cache_bits<16 do
            -- cache next 7 bits
            cache=cache | (((ord(sub(src,i,i))) or 0)&127)>>>16-cache_bits
            cache_bits+=7
            i+=1
        end
        -- clip out the bits we want
        -- and shift to integer bits
        local val=cache<<32-bits>>>16-bits
        -- now shift those bits out
        -- of the cache
        cache=cache>>>bits
        cache_bits-=bits
        return val
    end

    -- get number plus n
    local function gnp(n)
        local bits=0
        repeat
            bits+=1
            local vv=getval(bits)
            n+=vv
        until vv<(1<<bits)-1
        return n
    end

    -- header

    local 
        w,h_1,      -- w,h-1
        eb,el,pr,
        x,y,
        splen,
        predict
        =
        gnp"1",gnp"0",
        gnp"1",{},{},
        0,0,
        0
        --,nil

    for i=1,gnp"1" do
        add(el,getval(eb))
    end
    for y=y0,y0+h_1 do
        for x=x0,x0+w-1 do
            splen-=1

            if(splen<1) then
                splen,predict=gnp"1",not predict
            end

            local a=y>y0 and vget(x,y-1) or 0

            -- create vlist if needed
            local l=pr[a]
            if not l then
                l={}
                for e in all(el) do
                    add(l,e)
                end
                pr[a]=l
            end

            -- grab index from stream
            -- iff predicted, always 1

            local v=l[predict and 1 or gnp"2"]

            -- update predictions
            vlist_val(l, v)
            vlist_val(el, v)

            -- set
	    vset(x,y,v)

            -- advance
            x+=1
            y+=(x-x0)\w
            x%=w
        end
    end
end

-- [data structures]

-- vectors

vector = {}
vector.__index = vector

-- operators: +, -, *, /
function vector:__add(b) return v(self.x + b.x, self.y + b.y) end
function vector:__sub(b) return v(self.x - b.x, self.y - b.y) end
function vector:__mul(m) return v(self.x * m, self.y * m) end
function vector:__div(d) return v(self.x / d, self.y / d) end
function vector:__unm() return v(-self.x, -self.y) end

-- dot product
function vector:dot(v2) return self.x * v2.x + self.y * v2.y end

-- the # operator returns length squared since that's easier to
-- calculate
function vector:__len() return self.x^2 + self.y^2 end

-- creates a new vector with the x,y coords specified
function v(x, y) return setmetatable({ x = x, y = y }, vector) end


-- [objects and classes]

-- "object" is the base class for all other classes new classes are
-- declared by using object:extend({...})

object = {}

function object:extend(kob)
   kob = kob or {}
   kob.extends = self
   return setmetatable(kob, {
      __index = self,
      __call = function(self, ob)
	 ob = setmetatable(ob or {}, {__index = kob })
	 local ko, init_fn = kob
	 while ko do
	    if ko.init and ko.init ~= init_fn then
	       init_fn = ko.init
	       init_fn(ob)
	    end
	    ko = ko.extends
	 end
	 return ob
      end
   })
end


-- [collision boxes]

-- collision boxes are just axis-aligned rectangles
cbox = object:extend()

-- moves the box by the vector v and returns the result
function cbox:translate(v)
   return cbox({
	 xl = self.xl + v.x,
	 yt = self.yt + v.y,
	 xr = self.xr + v.x,
	 yb = self.yb + v.y
   })
end

-- checks if two boxes overlap
function cbox:overlaps(b)
   return self.xr > b.xl
      and b.xr > self.xl
      and self.yb > b.yt
      and b.yb > self.yt
 end

-- calculates a vector that neatly separates this box from
-- another. optionally takes a table of allowed directions
function cbox:sepv(b, allowed)
   local candidates = {
      v(b.xl - self.xr, 0),
      v(b.xr - self.xl, 0),
      v(0, b.yt - self.yb),
      v(0, b.yb - self.yt)
   }

   if type(allowed) ~= "table" then
      allowed = { true, true, true, true }
   end

   local ml, mv = 32767
   for d, v in pairs(candidates) do
      if allowed[d] and #v < ml then
	 ml, mv = #v, v
      end
   end

   return mv
end

-- makes a new box
function box(xl, yt, xr, yb)
   return cbox({
	 xl = min(xl, xr), xr = max(xl, xr),
	 yt = min(yt, yb), yb = max(yt, yb)
   })
end

-- makes a box from two corners
function vbox(v1, v2)
   return box(v1.x, v1.y, v2.x, v2.y)
end


-- [outlines]

-- from https://www.lexaloffle.com/bbs/?tid=32996
-- @alanxoc3
g_out_cache = {}

function init_out_cache(s_beg, s_end)
   for sind=s_beg, s_end do
      local bounds, is_bkgd = {}, function(x, y)
	 return mid(0, x, 7) == x and sget(x + sind * 8 % 128, y + flr(sind / 16) * 8) != 0
      end

      local calc_bound = function(x)
	 local top, bot

	 for i=0, 7 do
	    top, bot = top or is_bkgd(x, i) and i - 1, bot or is_bkgd(x, 7 - i) and 8 - i
	 end

	 return {top = top or 10, bot = bot or 0}
      end

      g_out_cache[sind] = {}
      for i = 0xffff, 8 do
	 -- prev, cur, next
	 local p, c, n = calc_bound(i - 1), calc_bound(i), calc_bound(i + 1)
	 local top, bot = min(min(p.top, c.top), n.top), max(max(p.bot, c.bot), n.bot)

	 if bot >= top then
	    add(g_out_cache[sind], {x1 = i, y1 = top, x2 = i, y2 = bot})
	 end
      end
   end
end

function spr_out(sind, x, y, sw, sh, xf, yf, col)
   local ox, x_mult, oy, y_mult = x, 1, y, 1
   if xf then ox, x_mult = 7 + x, 0xffff end
   if yf then oy, y_mult = 7 + y, 0xffff end

   foreach(g_out_cache[sind], function(r)
      rectfill(
	 ox + x_mult * r.x1,
	 oy + y_mult * r.y1,
	 ox + x_mult * r.x2,
	 oy + y_mult * r.y2 - 1, -- don't draw a bottom border, doesn't look great
	 col)
   end)

   spr(sind, x, y, sw, sh, xf, yf)
end


-- [effects]

function crectfill(x1, y1, x2, y2, c)
   if x1 < 128 and x2 > 0 and y1 < 128 and y2 > 0 then
      rectfill(max(0, x1), max(0, y1), min(127, x2), min(127, y2), c)
   end
end

function screen_shake()
   local fade, offset_x, offset_y = 0.95, 4 - rnd(8), 4 - rnd(8)
   offset_x *= offset
   offset_y *= offset

   camera(offset_x, offset_y)

   offset *= fade
   if (offset < 0.05) offset = 0
end

function white_fade(inc, fn)
   wfade = mid(0, (wfade or 16) + inc, 16)
   for x=1, 14 do
      for y=1, 14 do
	 fr = wfade * 10 / 16 - (x + y) / 30 * 4
	 fr = mid(0, fr, 6)
	 if fr < 2 then
	    rectfill(x * 8, y * 8, x * 8 + 8, y * 8 + 8, 0)
	 elseif fr < 5 then
	    circfill(x * 8 + 4, y * 8 + 4, fr, 7)
	 end
      end
   end
   if (wfade == 0) return fn()
end


-- [entities]

-- every entity has some basic properties entities have an embedded
-- state that control how they display and how they update each frame
-- if entity is in state "xx", its method "xx" will be called each
-- frame
entity = object:extend({
      state = "idle",
      t = 0,
      dynamic = true,
      spawns = {}
})

-- common initialization for all entity types
function entity:init()
   self.identifier = 'obj' .. ent_id

   if self.sprite then
      self.sprite = deep_copy(self.sprite)
      if (not self.render) self.render = spr_render
   end

   if self.lighting then
      self.blob_r = self.default_blob_r or 40
      blob_list[self.identifier] = {
	 p = {},
	 r = self.blob_r,
	 k = self.blob_r * self.blob_r / 2048,
	 v = {0, 0}
      }
   end

   ent_id += 1
end

function entity:update()
   if self.lighting then
      if self.blob_r + 0.5 < (blob_list[self.identifier].r or 0) then
	 local r = tlerp(self.blob_r, blob_list[self.identifier].r or 0, 0.9)
	 blob_list[self.identifier].r = r
	 blob_list[self.identifier].k = r * r / 2048
      end
      blob_list[self.identifier].p[1] = self.pos.x
      blob_list[self.identifier].p[2] = self.pos.y + 4
   end
end

-- called to transition to a new state - has no effect if the entity
-- was in that state already
function entity:become(state)
   if (state ~= self.state) self.state, self.t = state, 0
end

-- checks if entity has 'tag' on its list of tags
function entity:is_a(tag)
   if (not self.tags) return false

   for i=1, #self.tags do
      if (self.tags[i] == tag) return true
   end

   return false
end

-- called when declaring an entity class to make it spawn whenever a
-- tile with a given number is encountered on the level map
function entity:spawns_from(...)
   for tile in all({...}) do
      entity.spawns[tile] = self
   end
end

-- static entities never move like the level's walls - this lets us
-- optimize a bit, especially for collision detection
static = entity:extend({
      dynamic = false
})


-- [rendering from the "sprite" property]

function spr_render(e)
   local s, p = e.sprite, e.pos

   -- helper function for retrieving sprite data taking entity state
   -- into account, or a default value
   function s_get(prop, dflt)
      local st = s[e.state]
      if (st ~= nil and st[prop] ~= nil) then return st[prop] end
      if (s[prop] ~= nil) then return s[prop] end
      return dflt
   end

   -- sprite position
   local sp = p + s_get("offset", v(0, 0))

   -- width and height
   local w, h = s.width or 1, s.height or 1

   -- orientation and frames
   local flip_x, frames = false, s[e.state] or s.idle

   if s.turns then
      if e.facing == "up" then
	 frames = frames.u
      elseif e.facing == "down" then
	 frames = frames.d
      else
	 frames = frames.r
      end
      flip_x = (e.facing == "left")
   end

   if (s_get("flips")) flip_x = e.flipped

   -- animation
   local delay = frames.delay or 1
   if (type(frames) ~= "table") frames = {frames}
   local frm_index = flr(e.t / delay) % #frames + 1
   local frm = frames[frm_index]

   -- actual drawing
   spr_out(frm, round(sp.x), round(sp.y), w, h, flip_x)

   -- the current animation frame
   -- is returned, useful for
   -- custom :render() methods
   return frm_index
end


-- [entity registry]

-- entities are indexed for easy access. "entities" is a table with
-- all active entities. "entities_with.<property>" holds all entities
-- that have that property (used by various systems to find entities
-- that move, collide, etc.) "entities_tagged.<tag>" holds all
-- entities with a given tag, and is used for collisions, among other
-- things.

-- resets the entity registry
function entity_reset()
   entities, entities_with, entities_tagged, ent_id = {}, {}, {}, 0
end

-- registers a new entity, making it appear in all indices and update
-- each frame
function e_add(e)
   add(entities, e)
   for p in all(indexed_properties) do
      if (e[p]) index_add(entities_with, p, e)
   end

   if e.tags then
      for t in all(e.tags) do
	 index_add(entities_tagged, t, e)
      end
      c_update_bucket(e)
   end
   return e
end

-- removes an entity, effectively making it disappear
function e_remove(e)
   del(entities, e)
   for p in all(indexed_properties) do
      if (e[p]) del(entities_with[p], e)
   end

   if e.tags then
      for t in all(e.tags) do
	 del(entities_with[t], e)
	 if (e.bkt) del(c_bucket(t, e.bkt.x, e.bkt.y), e)
      end
   end
   e.bkt = nil
end

-- a list of properties that need an "entities_with" index
indexed_properties = {
   -- entites that should update each frame
   "dynamic",

   -- entities that render themselves or a hud
   "render", "render_hud",

  -- entities that move (have a velocity)
   "vel",

  -- entities that actively check for collisions
   "collides_with",

  -- entities that can be supported by a floor
   "feetbox"
}

-- [system: entity updating]

-- updates all entities according to their state
function e_update_all()
   for ent in all(entities_with.dynamic) do
      -- call the method with the name corresponding to this entity's
      -- current state
      local state = ent.state
      if (ent[state]) ent[state](ent, ent.t)

      if ent.done then
	 -- removed
	 e_remove(ent)
      elseif state ~= ent.state then
	 -- changed state, restart the "t" counter that tracks how
	 -- much time an entity has spent in its current state
	 ent.t = 0
      else
	 ent.t += 1
      end
   end
end

-- schedules a function to be called between udpates - needed for e.g. level changes that reset the entity indexes
function schedule(fn)
   scheduled = fn
end


-- [system: rendering the world]

function r_render_all(prop)
   -- collect all drawables and sort them into buckets separated by
   -- draw_order
   local drawables = {}
   for ent in all(entities_with[prop]) do
      local order = ent.draw_order or 0
      if (not drawables[order]) drawables[order] = {}
      add(drawables[order], ent)
   end

   -- render the drawable entities in the right order (z-indexing)
   for o = 0, 15 do
      for ent in all(drawables[o]) do
	 r_reset(prop)
	 ent[prop](ent, ent.pos)
      end
   end
end

-- helper function that resets pico-8 draw state before each entity
function r_reset(prop)
   palt(1, true)  -- extra transparent black to draw additional overlays
end


-- [system: movement]

function do_movement()
   for ent in all(entities_with.vel) do
      -- entities that have velocity move by that much each frame
      local ev = ent.vel
      ent.pos += ev

      -- orientation: flipped tracks left/right. 'true' is facing left
      if (ev.x ~= 0) ent.flipped = ev.x < 0

      -- facing: 4-direction facing, has a string value
      -- "right"/"left"/"up"/"down"
      if ev.x ~= 0 and abs(ev.x) > abs(ev.y) then
	 ent.facing = ev.x > 0 and "right" or "left"
      elseif ev.y ~= 0 then
	 ent.facing = ev.y > 0 and "down" or "up"
      end

      -- gravity affects velocity for all entities define a "weight"
      -- property
      if ent.weight then
	 local w = state_dependent(ent, "weight")
	 ent.vel += v(0, w)
      end
   end

   for ent in all(entities) do
      if ent.update then
	 ent:update()
      end
   end
end


-- [system: collision detection]

-- for efficiency, objects requiring collisions are sorted into 16x16
-- buckets based on their position

-- find bucket coordinates for entity "e"
function c_bkt_coords(e)
   local p = e.pos
   return flr(shr(p.x, 4)), flr(shr(p.y, 4))
end

-- get the bucket of entities with tag "t" at coords x,y
function c_bucket(t, x, y)
   local key = t .. ":" .. x .. "," .. y
   if (not c_buckets[key]) c_buckets[key] = {}
   return c_buckets[key]
end

-- updates bucket positions for dynamic entities
function c_update_buckets()
   for e in all(entities_with.dynamic) do
      c_update_bucket(e)
   end
end

-- actual bucket update for entity "e". takes care to only update when
-- needed, as switching buckets is costly.
function c_update_bucket(e)
   if (not e.pos or not e.tags) then return end
   local bx, by = c_bkt_coords(e)
   if not e.bkt or e.bkt.x ~= bx or e.bkt.y ~= by then
      if e.bkt then
	 for t in all(e.tags) do
	    del(c_bucket(t, e.bkt.x, e.bkt.y), e) -- old
	 end
      end
      e.bkt = v(bx, by)
      for t in all(e.tags) do
	 add(c_bucket(t, bx, by), e)
      end
   end
end

-- iterator that goes over all entities with tag "tag" that can
-- potentially collide with "e" - uses the bucket structure described
-- earlier.
function c_potentials(e, tag)
   local cx, cy = c_bkt_coords(e)
   local bx, by = cx - 2, cy - 1
   local bkt, nbkt, bi = {}, 0, 1

   return function()
      -- ran out of current bucket, find next non-empty one
      while bi > nbkt do
	 bx += 1
	 if (bx > cx + 1) then bx, by = cx - 1, by + 1 end
	 if (by > cy + 1) return nil
	 bkt = c_bucket(tag, bx, by)
	 nbkt, bi = #bkt, 1
      end
      -- return next entity in current bucket and increment index
      local e = bkt[bi]
      bi += 1
      return e
   end
end

-- resets the collision system, making all collision buckets empty
-- again
function collision_reset()
   c_buckets = {}
end

-- collision detection main function - detects all requested
-- collisions
function do_collisions()
   -- make sure our bucket structure is up to date
   c_update_buckets()

   -- iterate over all entities looking for collisions
   for e in all(entities_with.collides_with) do
      -- ...and all tags they're interested in
      for tag in all(e.collides_with) do
	 -- choose the more efficient path depending on how many
	 -- potential collisions there are
	 local nothers = entities_tagged[tag] and #entities_tagged[tag] or 0
	 if nothers > 4 then
	    -- for a large number of possible colliders, we iterate
	    -- over our bucket structure, since it's more efficient
	    for o in c_potentials(e, tag) do
	       if o ~= e then
		  -- get the colliders for each object
		  local ec, oc = c_collider(e), c_collider(o)
		  -- if both have one, check for collision between
		  -- them
		  if ec and oc then
		     c_one_collision(ec, oc)
		  end
	       end
	    end
	 else
	    -- for small numbers, we just iterate the entities directly
	    for oi = 1, nothers do
	       local o = entities_tagged[tag][oi]
	       -- quick check to rule out collisions quickly
	       local dx, dy = abs(e.pos.x - o.pos.x), abs(e.pos.y - o.pos.y)
	       if dx <= 20 and dy <= 20 then
		  -- quick check passed, do proper collisions using
		  -- hitboxes
		  local ec, oc = c_collider(e), c_collider(o)
		  if ec and oc then
		     c_one_collision(ec, oc)
		  end
	       end
	    end
	 end
      end
   end
end

-- manually check for collision between "box" and object with one of
-- the given "tags"
function c_check(box, tags)
   local fake_e = { pos = v(box.xl, box.yt) }
   for tag in all(tags) do
      for o in c_potentials(fake_e, tag) do
	 local oc = c_collider(o)
	 if oc and box:overlaps(oc.b) then
	    return oc.e
	 end
      end
   end
   return nil
end

-- checks for one collision and calls the reaction callbacks on each
-- object
function c_one_collision(ec, oc)
   if ec.b:overlaps(oc.b) then
      c_reaction(ec, oc)
      c_reaction(oc, ec)
   end
end

-- calls the :collide() method on a colliding object, if one
-- exists. if the return value is c_push_out or c_move_out, it acts on
-- that - separating the colliding entities by moving one of them.
function c_reaction(ec, oc)
   local reaction, param = event(ec.e, "collide", oc.e)
   if (type(reaction) == "function") reaction(ec, oc, param)
end

-- returns the collider for a given entity
function c_collider(ent)
   -- colliders are cached for efficiency, but they are only valid for
   -- one frame
   if ent.collider then
      if ent.coll_ts == g_time or not ent.dynamic then
	 return ent.collider
      end
   end

   -- nothing cached, create new collider
   local hb = state_dependent(ent, "hitbox")
   if (not hb) return nil
   local coll = {
      b = hb:translate(ent.pos),
      e = ent
   }

   -- cache it and return
   ent.collider, ent.coll_ts = coll, g_time

   return coll
end

-- reaction function, used by returning it from :collide(). cause the
-- other object to be pushed out so it no longer collides.
function c_push_out(oc, ec, allowed_dirs)
   local sepv = ec.b:sepv(oc.b, allowed_dirs)
   ec.e.pos += sepv
   if ec.e.vel then
      local vdot = ec.e.vel:dot(sepv)
      if vdot < 0 then
	 if (sepv.y ~= 0) ec.e.vel.y = 0
	 if (sepv.x ~= 0) ec.e.vel.x = 0
      end
   end
   ec.b = ec.b:translate(sepv)
end

-- inverse of c_push_out - moves the object with the :collide() method
-- out of the other object
function c_move_out(oc, ec, allowed)
   return c_push_out(ec, oc, allowed)
end


-- [system: support, basically objects being supported by floors]

function do_supports()
   -- entities that want support have a special collision box called the "feetbox"
   for e in all(entities_with.feetbox) do
      local fb = e.feetbox
      if fb then
	 -- look for support
	 fb = fb:translate(e.pos)
	 local support = c_check(fb, {"walls", "platform"})

	 -- if found, store it for later - entity update functions can
	 -- use the information
	 e.supported_by = support
      end
   end
end

-- [entity: level map]

-- the level entity only draws the level using map(). collisions are
-- taken care of by individual solid/support entities created on level
-- initialization
level = entity:extend({
      draw_order=1
})

function level:init()
   local b, s = self.base, self.size
   for x = 0, s.x - 1 do
      for y = 0, s.y - 1 do
	 -- get tile number
	 local blk = mget(b.x + x, b.y + y)

	 -- does this tile spawn an entity?
	 local eclass = entity.spawns[blk]
	 if eclass and eclass.tags[1] ~= "platform" then
	    -- yes, it spawns one let's do it!
	    local e = eclass({
		  pos = v(x, y) * 8,
		  tile = blk
	    })

	    -- register the entity
	    e_add(e)

	    -- replace the tile with empty space in the map
	    mset(b.x + x, b.y + y, 0)
	    blk = 0
	 end

	 -- check what type of tile this is
	 local btype = l_block_type(x, y, blk)
	 if btype then
	    -- it's not empty, so it gets an entity
	    local b = btype({
		  pos = v(x, y) * 8,
		  map_pos = b + v(x, y),
		  typ = bt
	    })

	    -- register only if needed (walls completely surrouned by
	    -- other walls aren't)
	    if (b.needed) e_add(b)
	 end
      end
   end
 end

-- renders the level
function level:render()
   -- palt(transparent_color, false)
   map(self.base.x, self.base.y, 0, 0, self.size.x, self.size.y)
end

function level:render_hud()
   -- draw collected items
   function draw_item(s, no_obj, dx)
      rectfill(dx, 122, dx + 19, 128, 0)
      spr(s, dx + 1, 120)
      print("\#0X", dx + 11, 122, 7)
      print("\#0" .. no_obj, dx + 17, 122, 7)
   end

   local n = 0
   if (has_treasure) n = 1
   draw_item(98, n, 14)
end   

-- solid blocks push everything out
solid = static:extend({
      tags = {"walls"},
      hitbox = box(0, 0, 8, 8)
})

-- supports are blocks you can stand on, but they don't push you out
-- if you jump from below them
support = solid:extend({
      tags = {"platform"},
      hitbox = box(0, 0, 8, 1)
})

function solid:init()
   self.allowed = { true, true, true, true }
   self.needed = true
end

-- solids push the player out
function solid:collide(e)
   return c_push_out, self.allowed
end

-- supports only push the player out conditionally
function support:collide(e)
end

-- block types depend on the sprite flags set in the sprite
-- editor. flag 0 set means a solid block, flag 1 - a support,
-- bridge-type block
function l_block_type(x, y, blk)
   if (fget(blk, 0)) return support
   if (shr(levels[level_index()][2][y + 1], 15 - x) & 1 == 1) return solid
   return nil
end


-- [entity: water]

water = entity:extend({
      tags = {"water"},
      hitbox = box(0, 1, 8, 8),
      collides_with = {"guy"},
      -- colors for surface animation
      colors = {7, 7, 0, 7, 7, 0}
})

water:spawns_from(19)

function water:init()
   -- initialize sine wave offsets for surface animation
   self.offsets = {}
   for x = 0, 7 do
      self.offsets[x] = rnd()
   end
end

function water:collide(o)
   if o:is_a("guy") then
      if (o.state != "drown") offset = 0.1
      o:become("drown")
   end
end

function water:render(p)
   -- first, the tile
   spr(self.tile, p.x, p.y)

   -- then, the 8 pixels on the surface will flash according to sine
   -- waves for a "wavy" appearance
   for x = 0, 7 do
      local sv = sin(self.t / 200 + self.offsets[x]) * 3 + 4
      local c = self.colors[flr(sv)]
      pset(p.x + x, p.y + 1, c)
   end
end

-- [entity: ladder]

-- the ladder entity actually takes care of all ladder-like objects -
-- ropes, vines, etc.
ladder = entity:extend({
      tags = {"ladder"},
      hitbox = box(2, -1, 6, 8)
})

-- that's why it spawns from all those tiles
ladder:spawns_from(162)

-- the entity only handles rendering - climbing happens in player
-- code
function ladder:render(p)
   spr(self.tile, p.x, p.y)
end

-- the ladder entity actually takes care of all ladder-like objects -
-- ropes, vines, etc.
platform = entity:extend({
      tags = {"platform"},
      hitbox = box(2, -1, 6, 8)
})

-- that's why it spawns from all those tiles
platform:spawns_from(250)

-- the entity only handles rendering - climbing happens in player
-- code
function platform:render(p)
   spr(self.tile, p.x, p.y)
end


-- [entity: player]
-- the main player entity
guy = entity:extend({
      tags = {"guy"},
      -- starts falling down
      state = "idle",
      -- moves
      vel = v(0, 0),
      -- affected by gravity, except when climbing
      weight = {0.2, climb = 0},
      -- various animations
      sprite = {
	 walk = {1, 3, delay = 5},
	 idle = {1, 9, 10, 9, delay = 30},
	 crouch = {8},
	 fly = {4},
	 dead = {8},
	 drown = {248},
	 climb = {6, 7, delay = 6, flips = false},
	 height = 1,
	 flips = true
      },
      draw_order = 11,
      -- collides with stuff
      collides_with = {"walls", "ladder", "platform"},
      hitbox = box(1, 0, 7, 7.999),
      -- has a feetbox to be able to stand on floors
      feetbox = box(1, 8, 7, 8.001),

      lighting = true,
      default_blob_r = 50
})

guy:spawns_from(1)

-- stores itself in a global when created - accessing player entity
-- easily from other code is very convenient
function guy:init()
   g_guy = self
end

-- flying
function guy:fly(t)
   -- altitude control for jumping the longer you hold the jump button,
   -- the higher the jump
   if btn(4) and not btn(3) and t < 3 and not self.dropping then
      self.vel.y = -1.75
   end

   -- air control - you can move left/right when airborne, but more
   -- sluggishly than on the ground
   if (btn(0)) self.vel -= v(0.3, 0)
   if (btn(1)) self.vel += v(0.3, 0)

   -- air resistance - limits both downward speed and the horizontal
   -- speed and jump length
   self.vel.x *= (1/1.3)

   -- different animation frames for jumping up/falling
   -- TODO: doesn't look up in sprites?
   self.sprite.fly[1] = self.vel.y > 0 and 5 or 4

   -- did we land?
   if self.supported_by and self.vel.y > 0 then
      self.vel = v(0, 0)
      self:become("idle")
   end

   -- did we hit a ladder?
   if (self.on_ladder and self.vel.y >= 0) self:become("climb")

   -- did we fall off-screen?
   if (self.pos.y > g_level.size.y * 8) self:kill()
end

-- from the idle state, we can start walking or jump
function guy:idle()
   self:do_walking()
   self:do_verticals()
   self:do_ladders()
end

-- from the walking state, we can continue walking, stop, or jump
function guy:walk(t)
   self:do_walking()
   self:do_verticals()
   self:do_ladders()
end

-- when crouching, we can't walk
function guy:crouch()
   if (not btn(3)) self:become("idle")

   -- slide if our velocity was non-zero when we crouched
   self.vel.x *= 0.8

   self:do_verticals()
end

-- "dead" state - dead players aren't removed to keep other code
-- working properly
function guy:dead()
   -- restart the level on button press
   if btnp(4) or btnp(5) then
      schedule(reset)
   end

   -- stop falling at some point to prevent the y-position from
   -- overflowing
   if self.pos.y > g_level.size.y * 8 + 50 then
      self.vel = v(0, 0)
      self.weight = 0
   end
end

function guy:drown()
   -- restart the level on button press
   if btnp(4) or btnp(5) then
      schedule(reset)
   end

   self.vel = v(0, 0.2)
   self.weight = 0
end

-- when climbing, you can move in all directions unaffected by gravity
function guy:climb()
   -- you can jump from ladders
   if btnp(4) then
      -- TODO: there is a glitch in animation for one frame when a guy
      -- jump off the ladder
      self.vel.y = -1.75
      -- we are jumping, but really falling
      self.on_ladder, self.dropping = false, true
      self:become("fly")
      return
   end

   -- when you move away from the ladder, you start falling
   if not self.on_ladder then
      self.dropping = true -- we we falling
      self:become("fly")
      return
   end

   -- 4-direction movement
   self.vel = v(0, 0)
   if btn(0) or btn(1) or btn(2) or btn(3) then
      if (btn(0)) self.vel -= v(0.6, 0)
      if (btn(1)) self.vel += v(0.6, 0)
      if (btn(2)) self.vel -= v(0, 0.6)
      if (btn(3)) self.vel += v(0, 0.6)
   end

   -- only animate while moving (dirty trick, sorry)
   if (#self.vel == 0) self.t -= 1

   -- reset the 'on_ladder' flag - it will be set back to true during
   -- collision detection if we're still on one
   self.on_ladder = false
end

-- collisions
function guy:collide(o)
   if o:is_a("ladder") or (o:is_a("platform")) then
      self.on_ladder = true
   end
end

-- getting killed turns off collisions and makes us dead
function guy:kill()
   self.vel, offset = v(0, -1), 0.01
   sfx(0, -1, 16, 4)
   self:become("dead")
end

-- common multiple-state stuff
function guy:do_walking()
   -- determine walking speed based on input
   self.vel = v(0, 0)
   if btn(0) or btn(1) then
      if (btn(1)) self.vel = v(1, 0)
      if (btn(0)) self.vel = v(-1, 0)
   end

   -- change state based on whether we're moving
   if self.vel.x ~= 0 then
      self:become("walk")
   else
      self:become("idle")
   end
end

function guy:do_verticals()
   -- did we fall down?
   if not self.supported_by then
      self:become("fly")
      return
   else
      self.dropping = false
   end

   -- are we jumping?
   if (btn(4)) then
      sfx(0, -1, 0, 6)
      self:become("fly")
      self.vel.y = -1.75
   end

   -- are we crouching?
   if (btn(3)) self:become("crouch")
end

function guy:do_ladders()
   -- should we start climbing?
   if self.on_ladder and (btn(2) or btn(3)) then
      self:become("climb")
   end
end

-- hud rendering
function guy:render_hud()
   if self.state == "dead" or self.state == "drown" then
      p("\#0press ❎ or 🅾️ to restart", 100, 7)
      p("\#0wake up?", 88, 7)
   end
end


-- [entity: ghost]
ghost = entity:extend({
      -- starts out going right
      tags = {"ghost"},
      state = "walking",
      vel = v(0.5, 0),
      -- affected by gravity
      weight = 0.2,
      -- has a few animated states
      sprite = {
	 aggro = {13},
	 walking = {11, 12, delay = 12},
	 charge = {13, 14, delay = 12},
	 fly = {11},
	 dead = {15},
	 height = 1,
	 flips = true
      },

      -- draws in front of most
      -- things
      draw_order = 10,

      -- has both hitbox and feetbox
      -- since it stands on floors
      collides_with = {"walls", "guy"},
      hitbox = {
	 box(1, 1, 7, 7.999),
	 dead = box(1, 2, 7, 7.999)
      },
      feetbox = box(1, 8, 7, 8.001),

      -- has an additional box it
      -- uses to check terrain in
      -- front of itself - to turn
      -- around before it falls
      floorbox = box(-1, 8, 1, 9),

      default_blob_r = 40,
      lighting = true
})

ghost:spawns_from(11)

-- default "patrol" state
function ghost:walking()
   -- feel for ground, turn
   -- around if it'd fall
   if (self:feel_around()) self:turn()

   -- look for player and
   -- become aggressive if
   -- you see him/her
   if (self:check_aggro()) self:become("aggro")
end

-- when aggro, the ghost
-- just stands in place
-- readying a charge
function ghost:aggro(t)
   self.vel, self.blob_r = v(0, 0), 75
   -- charge after 20 frames
   if (t >= 20) self:become("charge")
end

-- charging is like walking,
-- but faster
function ghost:charge()
   self.vel = v(self.flipped and -1 or 1, 0)

   -- we only turn around if we
   -- see a wall, but not if we're
   -- going to fall - letting the
   -- player goad ghost into
   -- falling from their platform
   if (self:feel_around() == "wall") self:turn()

   -- since we can actually
   -- fall, we should handle
   -- that
   self:do_falls()
end

-- fly state - when falling
-- from a platform after
-- a charge
function ghost:fly()
   -- did we land?
   if (self.supported_by) self:become("walking")

   -- did we fall off-screen?
   self.done = self.pos.y > g_level.size.y * 8
end

-- turning
function ghost:turn()
   self:become("walking")
   self.blob_r, self.vel = self.default_blob_r, self.flipped and v(0.5, 0) or v(-0.5, 0)
end

-- feeling around, returns
-- "wall" if a wall is in front,
-- "fall" if we ran out of
-- platform to walk on,
-- or nil if none of that
-- happened
function ghost:feel_around()
   local felt = nil
   if self.prev and #(self.prev - self.pos) == 0 then
      felt = "wall"
   elseif not self:check_ground() then
      felt = "fall"
   end
   self.prev = self.pos
   return felt
end

-- check for player - we become
-- aggressive when the player
-- is on roughly the same
-- level as us and close by
function ghost:check_aggro()
   local dy = g_guy.pos.y - self.pos.y
   if (abs(dy) > 2) return false
   local dx = g_guy.pos.x - self.pos.x
   return abs(dx) < 48 and (sgn(dx) == sgn(self.vel.x))
end

-- check if there is ground
-- in front of us - uses
-- the floorbox to query
-- the collision system
function ghost:check_ground()
   if not self.supported_by then
      return true
   end
   local projected_move = self.pos + v(sign(self.vel.x) * 4 + 4, 0)
   local box = self.floorbox:translate(projected_move)
   local c = c_check(box, {"walls"}) ~= nil
   return c
end

-- fall if not supported
-- by a floor
function ghost:do_falls()
   if (not self.supported_by) self:become("fly")
end

-- kill player on collision
function ghost:collide(o)
   -- 0.4 - ignore after ladder jump and walk over dead bodies
   if o:is_a("guy") and o.state != "dead" and o.vel.y > 0.4 and o.on_ladder == false then
      self:kill()
      o.vel, o.pos.y = v(0, -2), self.pos.y - 8
   elseif o:is_a("guy") and self.state != "dead" then
      o:kill()
   end
end

function ghost:kill()
   sfx(0, -1, 6, 2)
   self.vel, offset = v(0, 0), 0.01
   self:become("dead")
end

function ghost:dead()
   self.blob_r = 0
end


-- custom rendering - draws
-- the standard animated
-- sprite first, then draws
-- a "!" symbol on top if aggro
function ghost:render(p)
   spr_render(self)

   -- if we're aggroed, draw
   -- an exclamation point to
   -- alert the player
   if (self.state == "aggro") print("\#0!", self.pos.x + 3, self.pos.y - 7, 7)
end


-- [entity: door]
door = entity:extend({
      tags = {"door"},
      collides_with = {"guy"},
      hitbox = box(0, 0, 8, 8)
})

function door:render(p)
   local level = level_index()
   local door = find_door(p.x, p.y, level, true)

   if door then
      spr(175, p.x, p.y - 16)
      spr(238, p.x - 8, p.y - 8)
      spr(239, p.x + 8, p.y - 8)
   end

   spr(self.tile, p.x, p.y)
end

function door:update()
   self.collision = false
end

function door:collide(o)
   self.collision = true

   -- collides with player only
   if btnp(5) then
      wfade = 16
      sfx(0, -1, 8, 8)
      fade_fn = next_room
      freeze = 1
   end
end

function door:render_hud()
   if self.collision then
      local level = level_index()
      local door = find_door(self.pos.x, self.pos.y, level, true)

      local title = "???"
      if (door) title = levels[levels[level][3][door][3]][1]

      p("\#0press ❎ to enter", 100, 7)
      p("\#0" .. title, 108, 7)
   end
end

door:spawns_from(218)


-- [entity: skull]
skull = entity:extend({
      tags = {"skull"},
      hitbox = box(1, 5, 7, 8),
      collides_with = {"guy"},
      phrases = {
	 "run away!",
	 "you won't make it!",
	 "no one comes back alive!"
      },
})

function skull:init()
   self.phrase = flr(rnd(#self.phrases)) + 1
end

function skull:render(p)
   spr(self.tile, p.x, p.y)
end

function skull:update()
   self.is_talking = false
end

function skull:collide(o)
   self.is_talking = true
end

function skull:render_hud()
   if (self.is_talking) p("\#0" .. self.phrases[self.phrase], 100, 7)
end

skull:spawns_from(53)

-- [entity: bat]

-- bats go back and forth
-- without regard to gravity
bat = entity:extend({
      tags={"bat"},
      state="fly",
      -- animation, flips set to true
      -- since the bat is either
      -- going right or left
      sprite = {
	 fly = {16, 17, 18, 17, delay = 6},
	 flips = true
      },
      draw_order = 5,
      -- start out going to
      -- the right
      vel = v(0.5, 0),
      -- collisions
      collides_with = {"guy", "walls"},
      hitbox = box(2, 2, 6, 6),

      lighting = true,
      default_blob_r = 46
})

bat:spawns_from(16)

-- turn around on collision,
-- kill player on contact
function bat:collide(o)
   if o:is_a("walls") then
      self.vel = -self.vel
   elseif o:is_a("guy") and self.state != "dead" and self.state != "drown" then
      if o.vel.y > 0.3 and o.state != "climb" then
         sfx(0, -1, 6, 2)
	 o.vel, offset, o.pos.y = v(0, -1.5), 0.01, self.pos.y - 8
      else
	 o:kill()
      end
   end
end


-- [entity: fish]

-- fish go back and forth
-- without regard to gravity
fish = entity:extend({
      tags={"fish"},
      state="swim",
      sprite = {
	 swim = {246, 247, delay = 6},
	 flips = true
      },
      draw_order = 5,
      vel = v(0.5, 0),
      -- collisions
      collides_with = {"guy", "walls"},
      hitbox = box(2, 2, 6, 6),

      lighting = true,
      default_blob_r = 46
})

fish:spawns_from(246)

function fish:collide(o)
   if (o:is_a("walls")) self.vel = -self.vel
end


-- [entity: diamond]
treasure = entity:extend({
      tags = {"treasure"},
      collides_with = {"guy"},
      hitbox = box(2, 2, 6, 6)
})

treasure:spawns_from(98)

function treasure:update()
   self.collision = false
end

function treasure:collide(o)
   self.collision = true

   if btnp(5) then
      spiral_anim, offset = 10, 1
      has_treasure = true
      sfx(0, -1, 21,11)
      e_remove(self)
   end
end

function treasure:render_hud()
   if (self.collision) p("\#0press ❎ to pick up!", 100, 7)
end

function treasure:render(p)
   spr(self.tile, p.x, p.y)
end

-- [sfx]
-- jump: #0/0-5
-- ghost death: #0/6-7
-- select: #0/8-15
-- death: #0/16-19
-- diamond: #0/21-31


-- [helper functions]

function find_door(x, y, level, with_transition)
   local x, y = x / 8, y / 8
   local doors = levels[level][3]
   for i = 1, #doors do
      for dx = flr(x), flr(x) + 1, 1 do
	 for dy = flr(y), flr(y) + 1, 1 do
	    if doors[i][1] % 16 == dx and doors[i][2] % 16 == dy then
	       if with_transition then
		  if (doors[i][4]) return i
	       else
		  return i
	       end
	    end
	 end
      end
   end
end

-- centered text
function p(s, y, c)
   print(s, 68 - #s * 2, y, c)
end

function sign(x)
   if (x >= 0) return 1
   return -1
end

function maybe()
   return rnd() < 0.5
end

-- creates a deep copy of a table and all its properties
function deep_copy(obj)
   if (type(obj) ~= "table") then return obj end
   local cpy = {}
   setmetatable(cpy, getmetatable(obj))
   for k, v in pairs(obj) do
      cpy[k] = deep_copy(v)
   end
   return cpy
end

-- adds an element to an index, creating a new table in the index if
-- needed
function index_add(idx, prop, el)
   if (not idx[prop]) then idx[prop] = {} end
   add(idx[prop], el)
end

-- calls a method on an object, if it exists
function event(e, evt, p1, p2)
   local fn = e[evt]
   if (fn) return fn(e, p1, p2)
end

-- returns an entity's property depending on entity state e.g. hitbox
-- can be specified as { hitbox=box(...) } or { hitbox = {
--  walking=box(...),
--  crouching=box(...)
-- }
function state_dependent(e, prop)
   local p = e[prop]
   if (not p) return nil

   if type(p) == "table" and p[e.state] then
      p = p[e.state]
   end
   if type(p) == "table" and p[1] then
      p = p[1]
   end

   return p
end

function chance(a)
   return rnd() < a
end

-- round to nearest whole number
function round(x)
   return flr(x + 0.5)
end

function rndf(r1, r2)
   if (not r2) return call(rndf, r1)
   return r1 + (r2 - r1) * rnd()
end


-- [lighting]

dither = {0x0000.f,0x8000.f,0x8020.f,0xa020.f,0xa0a0.f,0xa4a0.f,0xa4a1.f,0xa5a1.f,0xa5a5.f,0xe5a5.f,0xe5b5.f,0xf5b5.f,0xf5f5.f,0xfdf5.f,0xfdf7.f,0xfff7.f,0xffff.f}

-- polyfill from
-- https://www.lexaloffle.com/bbs/?tid=3393
-- @fred72
function polyfill(p, c)
   if #p < 2 then return end

   local p0, nodes = p[#p], {}
   local x0, y0 = p0[1], p0[2]
   for i = 1, #p do
      local p1 = p[i]
      local x1, y1 = p1[1], p1[2]
      local _x1, _y1 = x1, y1
      if y0 > y1 then x0, y0, x1, y1 = x1, y1, x0, y0 end
      local dx = (x1 - x0) / (y1 - y0)
      if y0 < 0 then
	 x0 -= y0 * dx
	 y0 = 0
      end
      local cy0, cy1 = ceil(y0), ceil(y1)
      x0 += (cy0 - y0) * dx
      for y = cy0, min(cy1 - 1, 127) do
	 local x = nodes[y]
	 if x then
	    rectfill(x, y, x0, y, c)
	 else
	    nodes[y] = x0
	 end
	 x0 += dx
      end
      x0, y0 = _x1, _y1
   end
end

-- lighting from https://www.lexaloffle.com/bbs/?tid=43648
-- @electricgryphon
function init_field_grid()
   field_grid = {}
   for i = 0, 16 do
      field_grid[i] = {}
      for j = 0, 16 do
	 field_grid[i][j] = 0
      end
   end
end

function march_square(x, y, bright)
   local field_grid, case, threshold =
      field_grid,
      0,
      0.5

   local fg_tl, fg_tr, fg_br, fg_bl =
      field_grid[x][y],
      field_grid[x + 1][y],
      field_grid[x + 1][y + 1],
      field_grid[x][y + 1]

   if (fg_tl > threshold) case += 8
   if (fg_tr > threshold) case += 4
   if (fg_br > threshold) case += 2
   if (fg_bl > threshold) case += 1

   local sx, sy = x * 8, y * 8

   function customfill(case, tl, tr, bl, br, tm, rm, bm, lm, c)
      if case == 1 then polyfill({bl, lm, bm}, c)
      elseif case == 2 then polyfill({bm, rm, br}, c)
      elseif case == 3 then polyfill({lm, rm, br, bl}, c)
      elseif case == 4 then polyfill({tm, tr, rm}, c)
      elseif case == 5 then polyfill({bl, lm, tm, tr, rm, bm}, c)
      elseif case == 6 then polyfill({bm, tm, tr, br}, c)
      elseif case == 7 then polyfill({bl, lm, tm, tr, br}, c)
      elseif case == 8 then polyfill({lm, tl, tm}, c)
      elseif case == 9 then polyfill({bl, tl, tm, bm}, c)
      elseif case == 10 then polyfill({lm, tl, tm, rm, br, bm}, c)
      elseif case == 11 then polyfill({bl, tl, tm, rm, br}, c)
      elseif case == 12 then polyfill({lm, tl, tr, rm}, c)
      elseif case == 13 then polyfill({bl, tl, tr, rm, bm}, c)
      elseif case == 14 then polyfill({lm, tl, tr, br, bm}, c)
      end
   end

   if case == 0 then
      -- black block
      fillp()
      rectfill(sx, sy, sx + 8, sy + 8, 0)
   elseif case == 15 then
      -- a visible rectangle with a pattern fill
      fillp(dither[bright])
      rectfill(sx, sy, sx + 8, sy + 8, 0x70)
   else
      -- a visible polygon with a pattern fill
      local tl, tr, bl, br, tm, rm, bm, lm =
	 {sx, sy},
	 {sx + 8, sy},
	 {sx, sy + 8},
	 {sx + 8, sy + 8},
	 {sx + lerp(fg_tl, fg_tr, threshold) * 8, sy},
	 {sx + 8, sy + lerp(fg_tr, fg_br, threshold) * 8},
	 {sx + lerp(fg_bl, fg_br, threshold) * 8, sy + 8},
	 {sx, sy + lerp(fg_tl, fg_bl, threshold) * 8}

      -- a pattern fill
      fillp(dither[bright])
      rectfill(sx, sy, sx + 8, sy + 8, 0x70)

      -- a black clipping mask for a nice metablob effect
      fillp()
      customfill(15 - case, tl, tr, bl, br, tm, rm, bm, lm, 0)
   end
end

function tlerp(tar, pos, perc)
   return (1 - perc) * tar + perc * pos
end

function lerp(v1, v2, t)
   return (t - v1) / (v2 - v1)
end

function march_squares()
   local dither_step = 256 / #dither
   for i = 0, 15 do
      for j = 0, 15 do
	 march_square(i, j, flr(mid(field_grid[i][j] / 2, 0, 1) * #dither))
      end
   end
end

function generate_blobs()
   for i = 0, 15 do
      local x, i2 = i * 8, i / 2

      for j = 0, 15 do
	 local y, j2, v = j * 8, j / 2, 0

	 for k, blob in pairs(blob_list) do
	    local dx, dy = blob.p[1] / 16 - i2, blob.p[2] / 16 - j2
	    v += mid(blob.k / (dx * dx + dy * dy), 0, 2) -- dist
	 end

	 field_grid[i][j] = v
      end
   end
end


-- [levels]

function level_index()
   return room.y * 8 + room.x
end

-- TODO: DAG maze
function next_room()
   local level = level_index()

   if level == 4 then -- trap room
      local random_levels = { 1, 15, 2, 3 }
      level = random_levels[flr(rnd(4)) + 1]
   end

   local door = find_door(g_guy.pos.x, g_guy.pos.y, level, false)
   local new_level
   if door then
      levels[level][3][door][4] = true
      new_level = levels[level][3][door][3]
   end

   level = new_level or level

   load_room(level % 8, level \ 8)
end

function reset_particles(x, y)
   particles, spiral_particles, spiral_time, spiral_max = {}, {}, 0, 90

   for i = 0, 120 do
      add(particles, {
	     x = 8 + rnd(112),
	     y = 8 + rnd(112),
	     s = flr(rnd(1.25)),
	     spd = 0.05 + rnd(1),
	     off = rnd(),
	     c = 7,
      })
   end
end

function load_room(x, y)
   blob_list, wfade, fade_fn, freeze = {}, 0, nil, 0

   room.x, room.y = x, y

   -- reload map
   reload(0x2000, 0x2000, 0x1000)

   level_settings = {
      base = v(x * 16, y * 16),
      size = v(16, 16)
   }

   -- reset systems
   entity_reset()
   collision_reset()
   reset_particles(x, y)

   -- lighting
   init_field_grid()

   -- load level
   g_level = e_add(level(level_settings))

   local level = level_index()
   if level == 0 and has_treasure then
      -- game over
      is_credits = true

      tb_credits(0,
         {
	    'oh, wow. you managed to escape!\n\nit took you: ' .. h_time ..
	       '\n\ntry again?',
	    'credits:\n' ..
	       '◆ @mrmotarius: mrmotext\n   tiles and miniadventures\n' ..
	       '◆ @alanxoc3: sprite outline\n   function\n' ..
	       '◆ @fred72: polyfill function\n' ..
	       '◆ @electricgryphon: metablobs\n' ..
	       '◆ @cadars: 1-bit font\n' ..
	       '◆ @krajzeg: platformer\n   starter-kit\n' ..
	       '◆ @profpatonildo: textbox\n   functions\n' ..
	       '◆ celeste: particle-system\n\n' ..
	       'put everything together\n@oneearedrabbit\n\n' ..
	       'and you! thank you for playing!\n♥♥♥'
	 })
   else if (is_title == false) tb_init(0, levels[level][4])
   end
end


-- [text functions]

-- from https://www.lexaloffle.com/bbs/?pid=78868#p
-- by @profpatonildo
function tb_init(voice, string)
   reading = true
   tb = {
      str = string,
      voice = voice,
      i = 1,
      cur = 0,
      char = 0,
      x = 0,
      y = 106,
      w = 127,
      h = 21,
      col1 = 0, -- background color
      col2 = 7, -- border color
      col3 = 7, -- text color
   }
end

function tb_credits(voice, string)
   tb_init(voice, string)
   tb.y, tb.h = 0, 127
end

function tb_update()
   if tb.char < #tb.str[tb.i] then
      tb.cur += 0.5
      if tb.cur > 0.9 then
	 tb.char += 1
	 tb.cur = 0
	 -- if (ord(tb.str[tb.i], tb.char) != 32) sfx(tb.voice) -- play the voice sound effect.
      end
      if (btnp(5) or btnp(4)) tb.char = #tb.str[tb.i]
   elseif btnp(5) or btnp(4) then
      if #tb.str > tb.i then
	 tb.i += 1
	 tb.cur, tb.char = 0, 0
      else
	 reading = false
      end
   end
end

function tb_draw()
   if reading then
      rectfill(tb.x, tb.y, tb.x + tb.w, tb.y + tb.h, tb.col1)
      rect(tb.x, tb.y, tb.x + tb.w, tb.y + tb.h, tb.col2)
      print(sub(tb.str[tb.i], 1, tb.char), tb.x + 2, tb.y + 2, tb.col3)
      if (g_time < 15) then color(7)
      else color(0) end
      print("\#0❎", tb.x + tb.w - 8, tb.y + tb.h - 6)
      
   end
end

-- [main update loop]

function reset()
   h_time, g_time, t_time, seconds, minutes, room, reading = '00:00:00', 0, 0, 0, 0, v(0, 0), false
   freeze, is_title, is_credits = 0, true, false
   logo_x, logo_y = 37, 0
   has_treasure = false -- a global var to persist after level resets
   offset = 0 -- screen shake

   reset_particles(0, 0)
   spiral_anim, anim = 0, {1, 9, 10, 9} -- dummy title screen anims

   -- play title sound
   music(1)

   -- outlines
   r_reset()
   init_out_cache(1, 23)
   init_out_cache(246, 248)

   levels = {
      [0] = {
	 'entrance',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000100010001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1111111111111111,
	    0b1111111111111111,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {9, 11, 1, false}
	 },
	 {
	    "the maze. you walk through the\ngate into..."
	 }
      },
      [1] = {
	 'maze',
	 {
	    0b1111111111111111,
	    0b0000001000000001,
	    0b0000001000000001,
	    0b0000001000000001,
	    0b1110000111011111,
	    0b1100000000000111,
	    0b1000000000000111,
	    0b1000000000000111,
	    0b1000000000000111,
	    0b1111111110111111,
	    0b1111111100000001,
	    0b1111111100000001,
	    0b1111111100000001,
	    0b1111111100000001,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {18, 3, 15, false},
	    {29, 13, 3, false},
	    {24, 3, 0, false}
	 },
	 {
	    "...a room with a dusty floor.\nyou waited patiently and choose\nwhich way to go, into..."
	 }
      },
      [2] = {
	 'dungeon',
	 {
	    0b1111111111111111,
	    0b1100000000000011,
	    0b1100000000000011,
	    0b1100000010000011,
	    0b1111011101111111,
	    0b1100000000000011,
	    0b1100000000000011,
	    0b1111111111111011,
	    0b1100000000010011,
	    0b1100000000010011,
	    0b1100000000010011,
	    0b1110111101111011,
	    0b1100001000000011,
	    0b1100001000000011,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {44, 3, 4, false},
	    {40, 6, 3, false},
	    {37, 10, 13, false}
	 },
	 {
	    "...a gloomy, cavelike place\nfar underground. you followed\nto..."
	 }
      },
      [3] = {
	 'cave',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000011000111101,
	    0b1000100001111101,
	    0b1011000011110001,
	    0b1010001110000001,
	    0b1010011100000001,
	    0b1010111000000001,
	    0b1011111110111111,
	    0b1011111110111111,
	    0b1011111110111111,
	    0b1001100000001111,
	    0b1000100000001111,
	    0b1000100000001111,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {50, 3, 15, false},
	    {60, 7, 10, false},
	    {54, 13, 1, false},
	    {58, 13, 1, false}
	 },
	 {
	    "...a room with four doors. you\nhave been seeking for so long,\nand entered..."
	 }
      },
      [4] = {
	 'cliff',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1111100111111111,
	    0b1111000001111001,
	    0b1000000000110001,
	    0b1000000000110001,
	    0b1000000001100001,
	    0b1000000011100001,
	    0b1000000111110001,
	    0b1111111111111111,
	 },
	 {
	    {66, 7, 0, false}
	 },
	 {
	    "...an entirely different kind\nof place. there is only one\ndoor you moved on to..."
	 }
      },
      [5] = {
	 'trees',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1001000000000101,
	    0b1111110000011111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1111111111111111,
	 },
	 {
	    {85, 10, 2, false},
	    {91, 10, 7, false}
	 },
	 {
	    "...a beautiful garden. it is\n harder than it looks. after\na short rest, you came to..."
	 }
      },
      [6] = {
	 'guardians',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1100000000000011,
	    0b1100000000000011,
	    0b1101110011111011,
	    0b1100000000001011,
	    0b1100000000000011,
	    0b1100000000000011,
	    0b1100000000000011,
	    0b1100000000000011,
	    0b1100000000000011,
	    0b1100000000000011,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {100, 5, 11, false},
	    {106, 5, 4, false},
	    {103, 13, 9, false}
	 },
	 {
	    "...a large room with stone\nguardians you left this room\nand entered..."
	 }
      },
      [7] = {
	 'prison',
	 {
	    0b1111111111111111,
	    0b1111111000111111,
	    0b1111111000111111,
	    0b1100001001111111,
	    0b1100001000111111,
	    0b1100001000111111,
	    0b1100001000111111,
	    0b1101111000001101,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000011110111,
	    0b1110000110000011,
	    0b1111111110000011,
	    0b1111111110000011,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {116, 6, 13, false},
	    {122, 13, 8, false}
	 },
	 {
	    "...a much smaller room. you\nhurried out as quickly as you\ncould to..."
	 }
      },
      [8] = {
	 'temple',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000011111111,
	    0b1000000001111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000011111111,
	    0b1000000111111111,
	    0b1111111100000001,
	    0b1000000000000001,
	    0b1111111111111111,
	 },
	 {
	    {10, 26, 7, false},
	    {13, 26, 6, false},
	 },
	 {
	    "...a dramatic room with a\ntemple perhaps in another\nroom, you said, leaving for..."
	 }
      },
      [9] = {
	 'graveyard',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1111111111111111,
	    0b1111111111111111,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {24, 27, 5, false},
	    {29, 27, 6, false}
	 },
	 {
	    "...a spooky graveyard, with\nbackward looks you left room\nand entered..."
	 }
      },
      [10] = {
	 'castle',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000011110001,
	    0b1000000010000001,
	    0b1000000010000001,
	    0b1000000011111001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000011111111001,
	    0b1000000000000001,
	    0b1000011011011001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {39, 27, 11, false},
	    {42, 27, 13, false},
	    {41, 21, 4, false}
	 },
	 {
	    "...a castle. with few regrets\nyou left for..."
	 }
      },
      [11] = {
	 'tower',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000111101101,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1101000000000001,
	    0b1001100000111001,
	    0b1000110000000001,
	    0b1000011110000011,
	    0b1000010011111111,
	    0b1000010000000001,
	    0b1111110000000001,
	    0b1111111111111111,
	 },
	 {
	    {52, 24, 14, false},
	    {59, 24, 10, false},
	    {61, 19, 13, false},
	    {56, 19, 12, false}
	 },
	 {
	    "...a tower. quickly moving\nyou came to..."
	 }
      },
      [12] = {
	 'treasure room',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000010001,
	    0b1111111111111101,
	    0b1010000000000001,
	    0b1010000000000001,
	    0b1010110000011101,
	    0b1010000000000001,
	    0b1010000111000001,
	    0b1010001111100001,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {68, 25, 11, false},
	    {66, 22, 13, false}
	 },
	 {
	    "...a treasure room! you found\nit! you hurried to..."
	 }
      },
      [13] = {
	 'skull',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000010001001,
	    0b1000000010001001,
	    0b1000000010011001,
	    0b1000000011010001,
	    0b1000000001010001,
	    0b1000000001000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1111111111111111,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {93, 28, 4, false}
	 },
	 {
	    "...a room with a large\nfrightening skull. you rushed\nto..."
	 }
      },
      [14] = {
	 'snake',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1000000000000001,
	    0b1011110000000001,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {101, 28, 10, false},
	    {98, 28, 7, false}
	 },
	 {
	    "...a room that smelled\ndifferent, without rest you\ncame to..."
	 }
      },
      [15] = {
	 'platforms',
	 {
	    0b1111111111111111,
	    0b1000000000000001,
	    0b1100000000000001,
	    0b1100000000000001,
	    0b1100000000000001,
	    0b1100000000000001,
	    0b1111011000011011,
	    0b1111001011011011,
	    0b1111000001011011,
	    0b1110000000001011,
	    0b1110000000000011,
	    0b1110001100000011,
	    0b1110000000000011,
	    0b1111100011111111,
	    0b1111111111111111,
	    0b1111111111111111,
	 },
	 {
	    {117, 21, 2, false},
	    {121, 22, 4, false},
	    {119, 26, 13, false},
	    {123, 28, 9, false}
	 },
	 {
	    "...a room full of platforms\nyou choose one and went to..."
	 }
      }
   }
end

function _init()
   reset()
end

function _update()
   if (freeze > 0) return

   if scheduled then
      scheduled()
      scheduled = nil
   end

   g_time, t_time = (g_time + 1) % 30, (t_time + 1) % 120

   spiral_time += 1 / stat(8)
   rd = spiral_time -- * 36.88 -- for a spiral, 36.88 - trianglers

   if (reading) tb_update()

   if is_title then
      if btnp(4) or btnp(5) then
	 g_time = 0
	 is_title = false
	 music(2, 1000)
	 load_room(0, 0)
      end
   elseif is_credits then
      music(-1, 1000)
      if (btnp(4) or btnp(5)) and not reading then
	 reset()
      end
   else
      if g_time == 0 then
	 seconds = (seconds + 1) % 60
	 if (seconds == 0) minutes += 1
      end

      if not reading then
	 e_update_all()
	 do_movement()
	 do_collisions()
	 do_supports()

	 generate_blobs()
      end
   end
end


-- [drawing functions]

function draw_spiral(x, y)
   add(spiral_particles, {
	  pos = v(x, y),
	  vel = v(cos(rd), sin(rd))
   })

   add(spiral_particles, {
	  pos = v(x, y),
	  vel = v(cos(rd), sin(rd)) * 2
   })

   fillp(0b0101010110101010.1) -- lots of dots
   color(7)
   for j=1, 3 do
      line()
      for i=1, #spiral_particles - 2 do
	 local p = spiral_particles[i + j - 1]
	 p.pos += p.vel / 2
	 line(p.pos.x, p.pos.y)
      end
   end
   fillp()

   for j=1, 3 do
      if #spiral_particles > spiral_max then
	 local spiral_t = {}
	 for i=1, #spiral_particles - 1 do
	    spiral_t[#spiral_t + 1] = spiral_particles[i + 1]
	 end
	 spiral_particles = spiral_t
      end
   end
end   

function draw_logo(x, y)
   local data = "◝ト◝かレ∧チ⧗◝よヲタっ◆ヘオナんか⌂◆ナ🅾️き⌂サナユシ☉😐さ…す…けららるか★れぬ⬇️さら✽●◝⬆️▥けノきな⬆️ネ◝りユあノユそっユフナ⬇️⧗☉⬆️🐱れよ★くら░♪🅾️く🅾️ナかたを●✽ゆュˇ◝エハ◝かウニ◝ˇあラ⌂オきれ🐱かオニクきく⌂サ…✽つぬっ⬇️ヲひ☉░す…け😐ゃぬ➡️をら♥░けノ⬇️ニる●つノ➡️★✽∧ルユハ➡️さらふっわムっヨ▒るぬれ⌂✽わろ▒め█웃∧░●れっ⬇️░🐱ニ▒さく●れ█●웃る➡️くれ█ニ⌂きス…♥█ハり🐱♪⌂ュら◆ニ◝はンよイつまねも☉◜みノりユナりキュら◆め█っ⬇️らこニカ…れう█れひっ●●けむヌヘ🐱ムく☉そっろほまユネ☉セ☉✽∧も…さ☉ユ▤きヲぬ☉░ネかっく웃をイ…➡️☉ラ█★░ョらき░…チ●▤⬆️░🐱⬇️☉きスく⬇️😐ん…らぬく🅾️█あヘ☉⬇️⌂⬆️ニ✽ュ☉🅾️★ュかノ◝ふ"
   pal(11, 0, 1) -- transparent background
   px9_sdecomp(x, y, data, pget, pset)
end

function draw_borders()
   palt(0, false)  -- borders are opaque
   map(0, 0, 0, 0, 16, 1)
   map(0, 0, 0, 0, 1, 16)
   map(15, 0, 8*15, 0, 1, 16)
   map(0, 15, 0, 8*15, 16, 1)
   palt(0, true)
end

function title_screen()
   draw_spiral(64, 64)

   palt(1, false)
   pal(1, 0, 1)
   spr(anim[flr(t_time / 30) % 4 + 1], 60, 62)
   palt(1, true)

   p("\#0press ❎ or 🅾️ to start", 82, 7)
   p("\#0@oneearedrabbit, 2021", 112, 7)
end

function _draw()
   cls()
   fillp()

   local level = level_index()

   -- logo must be rendered before particles due to compression algo
   if is_title then
      draw_logo(logo_x, logo_y)
      if (logo_y < 19.5) logo_y = tlerp(logo_y, 20, 0.1)
   end

   -- particles
   foreach(particles, function(p)
	      p.x += p.spd
	      p.y += sin(p.off)
	      p.off += min(0.05, p.spd / 32)
	      crectfill(p.x, p.y, p.x + p.s, p.y + p.s, p.c)
	      if p.x > 120 then
		 p.x = 8
		 p.y = rnd(112) + 8
	      end
   end)

   if is_title then
      title_screen()
      draw_borders()
   elseif is_credits then
   else
      if spiral_anim > 0 and spiral_max > 0 then
	 draw_spiral(g_guy.pos.x, g_guy.pos.y)
	 if spiral_anim == 1 then
	    spiral_max -= 1
	    if (spiral_max <= 0) spiral_anim = 0
	 else
	    spiral_anim -= 1
	 end
      end
      screen_shake()

      -- main render
      r_render_all("render")

      -- spotlights
      march_squares()

      -- hud
      camera(0, 0)
      draw_borders()
      r_render_all("render_hud")

      local title = levels[level][1]
      local s, m = seconds, minutes % 60
      local h = flr(minutes / 60)
      h_time = (h < 10 and "0" .. h or h) .. ":" .. (m < 10 and "0" .. m or m) .. ":" .. (s < 10 and "0" .. s or s)

      print('\#0' .. title, 16, 1, 7)
      print('\#0' .. h_time, 82, 1, 7)

      if wfade > 0 then
	 white_fade(-1, fade_fn)
      end
   end

   -- check performance
   -- print(stat(1), 0, 0, 8)

   if (reading) tb_draw()
end


__gfx__
00000000077777000000000007777700077777700777770007777700077777000000000007777770000000000000000000000000000000000000000000000000
00000000777777700777770077777770777117107777777077777770777777700000000077777777077777700077777000777770007777700077777000000000
00000000777117107777777077711710777117107777777077777770777777700777770077117117777777770777777707777777077177170771771700777770
00000000777117107771171077711710777777707771171077777770777777707777777077117117777777770771771707717717077177170771771707777777
00000000077777707771171007777770077770707771171007777700077777007771170007777770771171170771771707717717077177170771771707111711
00000000000770700777777000007070007700000777777007777770777777007771170000077770077777700777777707777777077777770777777707777777
00000000007007000007707000070700777770000770707071111700071111700777777000700700000777700777777707777777077777770777777707777777
00000000007007000070070000707000000007000000000010000700070000100770707000700700007007000070707007070707007070700707070700707070
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000007700007000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00777700777777770770077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07707070077070700077770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07770770001707000070700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
07111170001111000017070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01000010000000000011110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000777000007770000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000070007700000077000770707000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000077077770770777700777777000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000007777177777771777771171000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000077777777077777777771171000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000077077770770777707777777000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000007700700077000777770000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000077000000770000000000000000000000000000000000000000000000000000000000000000000
__gff__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000
__map__
ad3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eae
4600d70000000000eceb000000000046460000000000b600000000000000004646b7000000000000000000000000b6464600000000000000000000000000104646000000000000000000000000000046460000000000000000000000000000464600000000000000000000000000004646b6b6b6b6b6b6eb0000b6b6b6b6b646
4600000000d700ea5e9e009f00ec50464600cf0000e0b7cecf0000000000004646b6000000000000e2000000cf00b6464600cf00b1c56a4e00b5b6b6b6b7fa4646000000000000000000000000000046460000000000000000000000000000464600000000000000000000000000004646b6b66564c79750ed8db6b6b6b6b646
46000000000000baf1c500c500d000464600da00eca7b6e1da0000000001004646b6000000000bb5b6b80000da00b6464600dab1bcac0000e0b6b6b6b6b6a24646000000000000000000e500eced0046460000000000000000000000000000464628000000000000000000000000294646b675dfd8a1a5baf3b6c7b6b6b6b646
46001000000000ba00b6b66a00f1ec4646b6b6fa5ee576b6b689fa88b6b6854646b6b6b6fab6b6c39ac2b6b6b7b7b64646fa88bcc30000a3b5b6b7647500a24646000000000000001000f1a75ef2004646ec50ed000000000000109800000046468c0000cf0000000000cf0000008d4646b600000000c45e0076b6b6b6b6b646
4600d70000008889000000888900d04646b675a25eba10ba5edfa200c268b64646b60000a200d400cf0000000000b64646a2b63a0000b6b6b63bdfde0000964646009400000000000000ecedd01c004646d01c5e002d2800000000f3eb4bed46468c00e3da0000000000da0001008d4646b60000cf00b6f3ed00b6b6b6b6b646
4600000000002eb6108a00b62fec5e4646a10096f3505050f400960000b6b64646b60000a2000000da0b00000000b64646a2b6ce00c8b6b4df001000cf000046464bcf4c000000000000f15ef200004646f3a7baec275e00000000ec50edba46468c0088a089000076b6b6b6b7fa8d4646b60000dab5b78cd000c4b6b6b6b746
460000ec50ed00b6251d22b600ba5e464600000000f1a7f20000000000b6c64646b7b6b6b6b6b6b4b6b6b6b6b6fab64646a2b68cb5b66a0000000000da00004646b5dab801000000e2e500dd009535464600eced5e1ad0000000005ebad0f246468c00009a00000000a1b379b4a28d4646b6fab6b6b37500f350f476d5b47546
4600eced00d000b6000000b6a750f446460000000b0000000000000000b6554646b60000000000000000d8b6e1a2b64646a2b6dbdcdbdcdb89fa88dbdcdbb84646b6b6b422000053b4b7b6b6b6b6b7464610f15ef2a7f400000000f3a75ea746468c0000000000001000000000a28d464600a200dfd4000000001000a5000046
4600f15ea7f400b6000000b600f1a74646b6b6b66ab6b6b6b6fab6d5b66a734646b6000000cf00000000e0b6cea2b64646a2b6b6b6b6b6b6b6a2b6b6b6b6b64646b6b6770000007278b6b6b7b780a1464600008100cf0000000000cfba830046468c0000420000000000410000a28d464600a200000000000000000000000146
4600ead0000000b600cf00b600000046465050505050edb6c3a200d30000004646b6000000da0b000000e0b6cea2b64646a2b6b6b6b4b6756aa2b6b7b6b7b6464679d300000000003dd6b764753b00464601b5b6b8da0000000000dab5b6b846468c00309f10000000cd9e3100a28d464600a200100000b5b6b66ab6fab65446
4601badd003543b600da00b64400004646b6b7b6b6b75eb600a200000000004646b6b6fab6b6b6b6fab7b6b46aa2b64646a2f066b700df000096d8a1b6b6b64646131313131313131313b4bb13131346462423a467220000100000258457d546468c8d58cb2800000029ca578ca28d4646b6b6b89de2b5b6b6df0000a24db746
465d5d5d6a5d5d5d5d5d5d5d5d5d6a4646b7b6b6b4a15eb600a2000000cf004646b600a20000b6009600000000a2b64646a200dfb600cf008b00cf00b6b6b64646f6000000000000006db6a60000004646133d3cdf131313131313132ca11b46468c86abab2f00cf002eabab87a28d4646b6b665b6b6b7b6b7e1cf009600b746
46b5b6b7b7b7b4b7b6b7b6b7b6b7b846465050505050f4b600a2000000da004646b700a201e0b6000000000b00a2b64646a2000168ceda0bd2e3da35b6b6b64646000000000000a36d65b78cf600004646000000000000000000000000001c46468c3871700b00da000071703a968d4646b6b658b7b66754b60bda00afe4b646
46b6b4b6b7b6b7b6b7b7b7b4b6b7b74646b6b6b7b6b6b6b6b6b6b6b6b7b6b6464666b7b6b6b6b6b6b6b6b4b6b6b7b64646b6b6b6b6b6b6b6b6b6b7b6b6b6b6464600e60000008d73b4b6b4b800000046460000000000000000000000f600004646245d5d5d5d5d5d5d5d5d5d5d5d244646b6b6b6b6b6b6b6b6b6b6b6b6b6b646
bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0
ad3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eaead3e3e3e3e3e3e3e3e3e3e3e3e3e3eae
46000000d700006d6e00000051520046460000000000000000000000000000464600000000000000007c00000000004646000000000000000000000000000046460000005319174a181600000000004646d0005eb5b6b6b6b6b7b6b6b6b6c74646000000000000000000002c3b00004646f15ebabaa75ef20000000000000046
4600d7000000007f80001051be56524646006d6e00001000000000000000004646ec5050eb00000051be5214000000464600000000000000cf00000000cf004646100000972a542a2a2b00006d6e004646f3a7bab6b6b6b6b6b6b6b7b6b6b74646000000000000000000002d00000046465cf3505050f4000000000000000046
460000000000000000d751be54bec74646007f8000000000000000000000004646ba00ec5050ed00b6b6b722000000464600ec50ed000000da00000000da004646000000532a2a54393a00007f80004646100086b6b6b7b676b6b6b6b6b6b64646000000ec50ed41e2e2e30000421046468c00f1f2de00000000000000000046
460000000000210000b1be54e9b7b646460000000000000000000000000000464650ed5ebaea5ea7b6cf75000000004646eced00d0004d5d5d5d6afa5d5dfa46460000382a542ad54a0000000000004646000053ec8fb6b6eccc6976b6b6b64646000000ba375e766457581ab6750046468c000000cf0000000000008b008b46
4600000000007e7ab1bec7b6b6b6b6464600000000000000000000001000004646bad4f350f4f350b6da32e23f3f004646f15ea7f4000000000000960000a2464600cf001b399a36a5344a190000004646eaa75a5e8ab65a5e008a00b6b6754646000000f1a7f350ed94ecb4a7ed004646b8010000da000000cf000025352246
4600d700000000c2bd54b6b6b6b6b6464600000000000000000000000000004646505050f210003db7b6b66a24fa004646ead0000000001c000000000010a24646e3da0b00e2e5174a846088ed01e24646ba00e8f310b6c9f37a91b5b7b63b464610000000de9b9c7473c7b6dbc7484646b6b68900b7b800e3da00b6b6fa9f46
4600000000000000c2dbdcdbdcdbdc464600000000100000000000000000004646f1f20000000000c765c72800a2004646badd00cf1000f3ed0000cf0000a24646b5b6b464b4b6b833b5b6b634b6fa464650ed0076617693b6b6fab675ebec464600000000eaa7ec50f475b7b675484646b6b7b6eb796a00b6b600b6b6a27546
46179900eced00d73d0000000000004646008a000000af0000008a00000000464600003f3fb51b1bb4575864b8a2004646b6fab8da00eced5e0000da0000a246468db61ecf1e1e26d540a61e37a1a24646bad400ba5ec8b6b6b6a2b600bad0464600000000f150f4a7a7ebb6f3eda34646b6b6b6ba100000a1b300b6b3968c46
4682f1a7d0f200001000cf0000cf0046462545220025452200254522004700464600006b25dcdbdcdbb7dbdcdba200464600a2a1b6b8f15ef20078b77700a2464600b600da000000d40000000000a24646505050f26cb6b6b7b6a2e110f3f4464600000000000000005fb5b6ecf4b64646b6b600ba0000cf0000007668a28c46
46007debd00000000000da0000da00464600460000004600cf00460000cf00464600006b00dcb7cfb6dccfdcdba200464600a2007868005e000000b7000096464600b6fa559f00000000009e56b7a24646f1f22d0000f9f9b3f9a20000000046460000000000000010b5b6b6f3edb64646c4b6a7f2009dda0000000000a28c46
4600f3ba3a100051b6b664b6b6b6b64646954601329546e5dae2469517da19464600001500a7a7daa7a7daa7a7a200464600a20000b7b7b7890b00b700b5b746468db7a2badf0000620000dfba00a2464600009200001d69001da20000cf00464600cf0000cf0000c8b6b6b4b6f4eb4646b5b60000009aa1001000cf00a28c46
460000dd01e551b6b6b6b6b4b6b6b646465d5d5d5d5d5d6a5d5d5d5d5db45d4646000090000000fa0000fa0000a200464600a20000b7c369a1b3b6b7b4b7b64646e0b6a25e000088c68900005e00a246460100950000000b0000960000da00464600da0000da00b5b4b77569b677ba4646b6b74200000000e3e200da0096b846
465d5d5d5d5d6ac16900000000000046461d1d1d1d1d1d1d1d1d1d1d1d1d1d4646000100000b00a20000a20000a200464600a20001b7e176757675767576754646b5b6a2c600b1b7b7b7b200c6009646465d5d6a5d5d222324242423255d6a4646b5b6b6b6b6b8d0004f0000d001d04646b6b6b6b6359d5ab6b6b4b6b6b6b346
4600000000000000000000000000004646a87b8e8e8e8e8e8e8e8e8e8e7baa4646de9ade9a9ade9a9a9a6f9ade9a9a4646b6b7b7b7b7e149484948494849484646b7b6b6b7b6b6b7b4b6b6b6b7b6b6464673645a5775000000000000a35a734646b6b6b4b6b6b6b6b6b6b4b664b6b64646b6b7b4b6b6b6b6b6b6b6b6b4b6b646
bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0bf3e3e3e3e3e3e3e3e3e3e3e3e3e3ec0
__sfx__
010400000c5400f5310f031180311b0211d0211d615063232471026710297102e7103071032710357103a7101c1431c1331c1231c1132f3202f3202f320343203432034320343203431034310343103431034315
011800000000006050060500605006050060500605006050060500605006050060400603500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011800000000012050120501205012050120501205012050120501205012050120401203500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011800000b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b02506020060250202002025090200902502020020250902009025
011800000b5250d5250e52510525125250e5251252012525115250d5251152011525105250c52510520105250b5250d5250e52510525125250e525125251752515525125250e5251252515520155201552015525
011800000b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b02506020060250202002025090200902502020020250902009025
011800000b5250d5250e52510525125250e5251252012525115250d5251152011525105250c52510520105250b5250d5250e52510525125250e525125251752515525125250e5251252515520155201552015525
011800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009520095200952009525
01180000060200602501020010250602006025010200102502020020250a0200a02506020060250d0200d025060200602501020010250602006025010200102502020020250a0200a02506020060250d0200d025
0118000006525085250a5250b5250d5250a5250d5200d5250e5250a5250e5200e5250d5250a5250d5200d52506525085250a5250b5250d5250a5250d5200d5250e5250a5250e5200e5250d5200d5200d5200d525
01180000060200602501020010250602006025010200102502020020250a0200a02506020060250d0200d025060200602501020010250602006025010200102502020020250a0200a02506020060250d0200d025
0118000012525145251652517525195251652519520195251a525165251a5201a5251952516525195201952512525145251652517525195251652519520195251a525165251a5201a52519520195201952019525
01180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d5200d5200d5200d525
011800000b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b02506020060250202002025090200902502020020250902009025
011800000b5250d5250e52510525125250e5251252012525115250d5251152011525105250c52510520105250b5250d5250e52510525125250e525125251752515525125250e5251252515520155201552015525
011800000b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b02506020060250b0200b0250b0250b025090200902507020070250602006025040200402502020020250102001025
011800000b5250d5250e52510525125250e5251252012525115250d5251152011525105250c52510520105250b5250d5250e52510525125250e5251252517525125250e52512525175250b5200b5200b5200b525
011800000b0500b05506050060550b0500b05506050060550b0500b05506050060550b0500b05506050060550b0500b05506050060550b0500b05506050060550e0500e05515050150550e0500e0551505015055
0118000000000000000e0550000000000000000e0550000000000000000d0550000000000000000d0550000000000000000e0550000000000000000e055000000000000000120550000000000000001205500000
0118000017555195551a5551c5551e5551a5551e5501e5551d555195551d5501d5551c555185551c5501c55517555195551a5551c5551e5551a5551e55523555215551e5551a5551e55521550215502155021555
011800000000000000125550000000000000001255500000000000000012555000000000000000125550000000000000001255500000000000000000000000000000000000155550000000000000001555500000
011800000b0500b05507050060550b0500b05506050060550b0500b05506050060550b0500b05506050060550b0500b05506050060550b0500b05506050060550b0500b05509050090550b0500b0550905009055
0118000000000000000e0550000000000000000e0550000000000000000d0550000000000000000c0550000000000000000e0550000000000000000e0550000000000000000e0550000000000000000e05500000
0118000017555195551a5551c5551e5551a5551e5501e5551d555195551d5501d5551c555185551c5501c55517555195551a5551c5551e5551a5551e55523555215551e5551a5551e55521550215502155021555
011800000000000000125550000000000000001255500000000000000012555000000000000000125550000000000000001255500000000000000000000000000000000000155550000000000000001555500000
0118000006050060550d0500d05506050060550d0500d0550205002055120501205506050060550d0500d05506050060550d0500d05506050060550d0500d0550205002055120501205506050060550d0500d055
011800000000000000160550000000000000001605500000000000000016055000000000000000160550000000000000001605500000000000000016055000000000000000160550000000000000001605500000
0118000012555145551655517555195551655519550195551a555165551a5501a5551955516555195501955512555145551655517555195551655519550195551a555165551a5501a55519550195501955019555
011800000000000000000000000000000000001655016555000000000016550165550000000000165501655500000000001255012555000000000016550165550000000000165501655500000000001655016555
0118000006050060550d0500d05506050060550d0500d05502050020550e0500e05506050060550d0500d05506050060550d0500d05506050060550d0500d05502050020550e0500e05506050060550d0500d055
011800000000000000160550000000000160550000000000000000000016055000000000016055000000000000000000001605500000000001605500000000000000000000160550000000000160550000000000
011800001e5552055522555235552555522555255502555526555225552655026555255552255525550255551e555205552255523555255552255525550255552655522555265502655525550255502555025555
0118000000000000001e5550000000000000001e5550000000000000001e5550000000000000001e5550000000000000001e5550000000000000001e5550000000000000001e5550000000000000001e55500000
011800000b070060700e070060700b070060700e070060700b070060700d070060700b070060700c070060700b070060700e070060700b070060700e070060700207009070120700907002070090701207009070
011800000b5750d5750e57510575125750e5751257012575115750d5751157011575105750c57510570105750b5750d5750e57510575125750e575125751757515575125750e5751257515570155701557015575
01180000000000000006570000000000000000065700000000000000000657000000000000000006570000000000000000065700000000000000000b5703c0000000000000095700000000000000000957000000
011800000b070060700e070060700b070060700e070060700b070060700d070060700b070060700c070060700b070060700b0700b070090700907007070070700607006070040700407002070020700107001070
0118000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000b0700b0750b0700b0750b0700b0750b0700b0750b0700b0750b0700b075
01180000000000000012570125700000000000125701257000000000001257012570000000000012570125700000000000170701707015070150701307013070120701207010070100700e0700e0700d0700d070
0118000017575195751a5751c5751e5751a5751e5701e5751d575195751d5701d5751c575185751c5701c57517575195751a5751c5751e5751a5751e575235751e5751a5751e5752357517570175701757017575
011800000b0750907507075060750b0750907507075060750b0750907507075060750b0750907507075060750b0750907507075060750b07509075070750607502075000750b07509075020750c0750b07509075
0118000017575195751a5751c5751e5751a5751e5701e5751d5751a5751d5701d5751c575185751c5701c57517575195751a5751c5751e5751a5751e57523575215751e5751a5751e57521570215702157021575
011800001757017570175701757517570175701757017575175701757017570175751757017570175701757517570175701757017575175701757017570175751557015570155701557515570155701557015570
011800001707500000000000000017075000000000000000170750000000000000001707500000000000000017075000000000000000170750000000000000001a5701a5701a5701a5751a5701a5701a5701a570
011800000b0750907507075060750b0750907507075060750b0750907507075060750b0750907507075060750b0750907507075060750b0750907507075060750b0750907507075060750b075090750707506075
0118000017575195751a5751c5751e5751a5751e5701e5751d575195751d5701d5751c575185751c5701c57517575195751a5751c5751e5751a5751e57523575215751e5751a5751e57521570215702157021575
011800001757017570175701757517570175701757017575175701757017570175751757017570175701757517570175701757017575175701757017570175751557015570155701557515570155701557015575
011800001707500000000000000017075000000000000000170750000000000000001707500000000000000017075000000000000000170750000000000000001a5750000000000000001a575000000000000000
011800000607504075020750107506075040750207501075020750107502075040750607504075020750107506075040750207501075060750407502075010750207500075020750407506075040750207501075
011800001e5752057522575235752557522575255702557526575225752657026575255752257525570255751e575205752257523575255752257525570255752657522575265702657525570255702557025575
011800001e5701e5751e5701e5751e5701e5701e5701e5751e5701e5751e5701e5751e5701e5751e5701e5751e5751e5751e5751e5751e5751e5751e5751e5751e5751e5751e5751e5751e5751e5751e5751e575
01180000000000000000000000002557500000255752400026575000002657500000255750000025575240000e055000000000000000255750000025575005002657500000225750000026575000001957500000
011800000607505075030750107506075050750307501075030750107503075050750607505075030750107503075050750307501075030750507503075010750607501075030750507506075050750307501075
011800001e5752057522575235752557522575255702557527575225752757027575255752257525570255751e575205752257523575255752257525570255752757522575275702757525570255702557025575
011800001e5701e5701e5701e5751e5701e5751e5701e5751e0701e0751e0701e0751e0701e0751e0701e075120701e5751e5751e5751e5751e5751e5751e5751e5751e5751e5701e5751e5701e5701e5701e575
01180000000000000000000000000d5700d5750d5700d5750f5700f5750f5700f5750d5700d5750d5700d575000000000000000000000d5750d5750d5700d5750f5750f5750f5700f5750d5700d5700d5700d575
011800000b0750907507075060750b0750907507075060750b0750907507075060750b0750907507075060750b0750907507075060750b07509075070750607502075010750b0750907502075010750b07509075
0118000017575195751a5751c5751e5751a5751e5701e5751d5751a5751d5701d5751c575185751c5701c57517575195751a5751c5751e5751a5751e57523575215751e5751a5751e57521570215702157021575
011800001757017570175701757517570175701757017575175701757017570175751757017570175701757517570175701757017575175701757017570175751557015570155701557515570155701557015575
011800001707500000000000000017075000000000000000170750000000000000001707500000000000000017075000000000000000170750000000000000000e0750000000000000000e075000000000000000
011800000b0700b07500000000000d0700d07500000000000b0700b07500000000000e0700e075000000000007070070750e0700e07507070070750e0700e07507070070750d0700d0750b0700b0750000000000
01180000170701707500000000001e0701e0750000000000170701707500000000001e0701e075000000000013070130701707017070120701207017070170701207012070120701207017070170700000000000
011800000000000000205702257023570235750000000000180001800020570225702357023575000000000017575195751a5751c5751e5751a5751e57523575225751e575225752557523570235702357023570
011800000000000000000000000017570175750000000000000000000000000000001757017575000000000000000000001307013070000000000012070120700000000000120701207000000000001207012070
__music__
01 41424344
05 01024344
01 03044748
00 0506074c
00 08094f50
00 0a0b0c54
00 0d0e5758
00 0f104344
00 11121314
00 15161718
00 191a1b1c
00 1d1e1f20
00 21222364
00 24252627
00 28292a2b
00 2c2d2e2f
00 30313233
00 34353637
00 38393a3b
02 3c3d3e3f

