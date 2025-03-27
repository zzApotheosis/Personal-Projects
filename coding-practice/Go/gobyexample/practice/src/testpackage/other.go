package other

import ()

type Pencil struct {
	graphite int
	version  string
}

func (p *Pencil) Init() {
	p.graphite = 100
	p.version = "#2"
}

func (p *Pencil) SetGraphite(amount int) {
	p.graphite = amount
}

func (p *Pencil) IncGraphite(amount int) {
	p.SetGraphite(p.graphite + amount)
}

func (p *Pencil) DecGraphite(amount int) {
	p.SetGraphite(p.graphite - amount)
}

func (p *Pencil) SetVersion(name string) {
    p.version = name
}
