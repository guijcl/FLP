package main

import (
	"fmt"
)

type Json struct {
	Number *struct {
		int
	}
	String *struct {
		string
	}
	Boolean *struct {
		bool
	}
	Array *struct {
		arr []Json
	}
	Object *struct {
		obj map[string]Json
	}
	Null *struct{}
}

type JC struct {
	Number *struct {
		int
	}
	String *struct {
		string
	}
	Boolean *struct {
		bool
	}
	Null    *struct{}
	Channel *struct {
		ch chan JC
	}
	VS *bool
	OS *bool
}

type Accessor struct {
	LabelAccess *struct {
		Label    string
		Accessor *Accessor
	}
	ArrayAccess *struct {
		Index    int
		Accessor *Accessor
	}
	Map *struct {
		Accessor *Accessor
	}
	End *struct{}
}

func serialise_json(json *Json, sender chan JC, quit chan string) {
	rec_serialise_json(json, sender)
	quit <- "done"
}

func rec_serialise_json(json *Json, sender chan JC) {
	switch {
	case json.Number != nil:
		sender <- JC{Number: &struct{ int }{json.Number.int}}
	case json.String != nil:
		sender <- JC{String: &struct{ string }{json.String.string}}
	case json.Boolean != nil:
		sender <- JC{Boolean: &struct{ bool }{json.Boolean.bool}}
	case json.Array != nil:
		v_ch := make(chan JC)
		sender <- JC{Channel: &struct{ ch chan JC }{v_ch}}
		v_ch <- JC{VS: new(bool)}
		v_ch <- JC{Number: &struct{ int }{len(json.Array.arr)}}
		for _, e := range json.Array.arr {
			rec_serialise_json(&e, v_ch)
		}
		close(v_ch)
	case json.Object != nil:
		o_ch := make(chan JC)
		sender <- JC{Channel: &struct{ ch chan JC }{o_ch}}
		o_ch <- JC{OS: new(bool)}
		o_ch <- JC{Number: &struct{ int }{len(json.Object.obj)}}
		for key, value := range json.Object.obj {
			o_ch <- JC{String: &struct{ string }{key}}
			rec_serialise_json(&value, o_ch)
		}
		close(o_ch)
	case json.Null != nil:
		sender <- JC{Null: &struct{}{}}
	}
}

func deserialise_json(receiver chan JC, quit chan string) Json {
	json := rec_deserialise_json(<-receiver, receiver)
	print_json(json)
	quit <- "done"
	return json
}

func rec_deserialise_json(jc JC, receiver chan JC) Json {
	switch {
	case jc.Number != nil:
		return Json{Number: jc.Number}
	case jc.String != nil:
		return Json{String: jc.String}
	case jc.Boolean != nil:
		return Json{Boolean: jc.Boolean}
	case jc.Null != nil:
		return Json{Null: nil}
	case jc.Channel != nil:
		var next_val = <-jc.Channel.ch
		return rec_deserialise_json(next_val, jc.Channel.ch)
	case jc.VS != nil:
		var arr = make([]Json, 0)
		var length = get_len(<-receiver)
		for i := 0; i < length; i++ {
			var elem = rec_deserialise_json(<-receiver, receiver)
			arr = append(arr, elem)
		}
		return Json{Array: &struct{ arr []Json }{arr}}
	case jc.OS != nil:
		var obj = make(map[string]Json)
		var length = get_len(<-receiver)
		for i := 0; i < length; i++ {
			var key = get_string_json(rec_deserialise_json(<-receiver, receiver))
			var value = rec_deserialise_json(<-receiver, receiver)
			obj[key] = value
		}
		return Json{Object: &struct{ obj map[string]Json }{obj}}
	default:
		panic("An invalid Json Channel value was sent")
	}
}

func eval(accessor *Accessor,
	receiver <-chan JC, sender chan<- JC,
	quit chan string) {
	rec_eval(<-receiver, accessor, receiver, sender)
	quit <- "done"
}

func rec_eval(jc JC, accessor *Accessor, receiver <-chan JC, sender chan<- JC) {
	switch {
	case jc.Number != nil:
		sender <- JC{Number: jc.Number}
	case jc.String != nil:
		sender <- JC{String: jc.String}
	case jc.Boolean != nil:
		sender <- JC{Boolean: jc.Boolean}
	case jc.Null != nil:
		sender <- JC{Null: jc.Null}
	case jc.Channel != nil:
		var tmpReceiver = jc.Channel.ch
		rec_eval(<-tmpReceiver, accessor, tmpReceiver, sender)
	case jc.VS != nil:
		var length = <-receiver
		switch {
		case accessor.ArrayAccess != nil:
			for i := 0; i < get_len(length); i++ {
				value := <-receiver
				if i == accessor.ArrayAccess.Index {
					sender <- value
				} else {
					if value.Channel != nil {
						for d_value := range value.Channel.ch {
							discard(d_value)
						}
					}
				}
			}
		case accessor.Map != nil:
			v_ch := make(chan JC)

			sender <- JC{Channel: &struct{ ch chan JC }{v_ch}}
			v_ch <- JC{VS: new(bool)}
			v_ch <- length

			for v_jc := range receiver {
				rec_eval(v_jc, accessor.Map.Accessor, receiver, v_ch)
			}
		case accessor.End != nil:
			v_ch := make(chan JC)

			sender <- JC{Channel: &struct{ ch chan JC }{v_ch}}
			v_ch <- JC{VS: new(bool)}
			v_ch <- length

			for v_jc := range receiver {
				rec_eval(v_jc, accessor, receiver, v_ch)
			}
		default:
			panic("Accessor not applicable")
		}
	case jc.OS != nil:
		length := <-receiver
		switch {
		case accessor.LabelAccess != nil:
			length := get_len(length)
			for i := 0; i < length; i++ {
				var key = get_string_jc(<-receiver)
				var value = <-receiver
				if key == accessor.LabelAccess.Label {
					rec_eval(value, accessor.LabelAccess.Accessor, receiver, sender)
				} else {
					if value.Channel != nil {
						for d_value := range value.Channel.ch {
							discard(d_value)
						}
					}
				}
			}
		case accessor.End != nil:
			o_ch := make(chan JC)

			sender <- JC{Channel: &struct{ ch chan JC }{o_ch}}
			o_ch <- JC{OS: new(bool)}
			o_ch <- length

			for o_jc := range receiver {
				o_ch <- o_jc
				rec_eval(<-receiver, accessor, receiver, o_ch)
			}
		default:
			panic("Accessor not applicable")
		}
	default:
		panic("An invalid Json Channel value was sent")
	}
}

func main() {
	json := Json{Object: &struct{ obj map[string]Json }{map[string]Json{
		"name":       {String: &struct{ string }{"Jason Ray"}},
		"profession": {String: &struct{ string }{"Software Engineer"}},
		"age":        {Number: &struct{ int }{31}},
		"address": {Object: &struct{ obj map[string]Json }{map[string]Json{
			"city":       {String: &struct{ string }{"New York"}},
			"postelCode": {Number: &struct{ int }{64780}},
			"country":    {String: &struct{ string }{"USA"}}}}},
		"languages": {Array: &struct{ arr []Json }{[]Json{
			{String: &struct{ string }{"Java"}},
			{String: &struct{ string }{"Node.js"}},
			{String: &struct{ string }{"JavaScript"}},
			{String: &struct{ string }{"JSON"}}}}},
		"socialProfiles": {Array: &struct{ arr []Json }{[]Json{
			{Object: &struct{ obj map[string]Json }{map[string]Json{
				"name": {String: &struct{ string }{"Twitter"}},
				"link": {String: &struct{ string }{"https://twitter.com"}}}}},
			{Object: &struct{ obj map[string]Json }{map[string]Json{
				"name": {String: &struct{ string }{"Facebook"}},
				"link": {String: &struct{ string }{"https://facebook.com"}}}}}}}}}}}

	//accessor := &Accessor{End: &struct{}{}}
	/*accessor := &Accessor{
		LabelAccess: &struct {
			Label    string
			Accessor *Accessor
		}{
			Label:    "socialProfiles",
			Accessor: &Accessor{End: &struct{}{}},
		},
	}*/
	/*accessor := &Accessor{
		LabelAccess: &struct {
			Label    string
			Accessor *Accessor
		}{
			Label: "socialProfiles",
			Accessor: &Accessor{
				ArrayAccess: &struct {
					Index    int
					Accessor *Accessor
				}{
					Index:    0,
					Accessor: &Accessor{End: &struct{}{}},
				}},
		},
	}*/
	accessor := &Accessor{
		LabelAccess: &struct {
			Label    string
			Accessor *Accessor
		}{
			Label: "socialProfiles",
			Accessor: &Accessor{
				Map: &struct{ Accessor *Accessor }{
					Accessor: &Accessor{
						LabelAccess: &struct {
							Label    string
							Accessor *Accessor
						}{
							Label:    "name",
							Accessor: &Accessor{End: &struct{}{}}}}}},
		},
	}

	ch := make(chan JC)
	ch1 := make(chan JC)

	quit := make(chan string)

	go serialise_json(&json, ch, quit)
	go eval(accessor, ch, ch1, quit)
	go deserialise_json(ch1, quit)

	for i := 0; i < 3; i++ {
		<-quit
	}
}

// FUNÇÕES AUXILIARES
func print_json(json Json) {
	switch {
	case json.Number != nil:
		fmt.Print(json.Number.int)
	case json.String != nil:
		fmt.Print(json.String.string)
	case json.Boolean != nil:
		fmt.Print(json.Boolean.bool)
	case json.Array != nil:
		fmt.Print("[")
		for _, e := range json.Array.arr {
			print_json(e)
			fmt.Print(", ")
		}
		fmt.Print("]")
	case json.Object != nil:
		fmt.Println("\n{")
		for k, v := range json.Object.obj {
			fmt.Print(k)
			print_json(v)
			fmt.Println(", ")
		}
		fmt.Println("}")
	case json.Null != nil:
		fmt.Print("null")
	}
}

func get_string_json(json Json) string {
	switch {
	case json.String != nil:
		return json.String.string
	default:
		return "Not a String"
	}
}

func get_string_jc(jc JC) string {
	switch {
	case jc.String != nil:
		return jc.String.string
	default:
		return "Not a String"
	}
}

func get_len(jc JC) int {
	switch {
	case jc.Number != nil:
		return jc.Number.int
	default:
		return -1
	}
}

func discard(jc JC) {

}
