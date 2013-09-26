
----

# Post-Mortem { #post-mortem }

<!-- Image taken from http://vimeo.com/amegraphic -->

<div class="center nopad"> ![Post-Mortem](resources/images/post-mortem.png) </div>

<div class='note notitle'>

## tl;dr

* Background colors allowed a birds-eye view of progress
* Volunteers rock - One on one time is invaluable
* Make sure your building has water
* Buy beer for your volunteers

</div>

We ran the Haskell workshop on the 21st of September, 2013 from 10am until 5pm,
with an hour for lunch at 12:30pm. Responsibilities were split into
three categories with name-tag colorings available to match:

* Attendee
* Volunteer
* Admin / Contributor

After running the workshop and having a few days to reflect, there are
many aspects that warrant analysis. Some things could have been run
more efficiently, or scrapped all-together. Certain administrative
processes could have been arranged in a manner that avoided some pitfalls.
There were also some non-conventional approaches adopted that worked
far better than anticipated, and these deserve some discussion.
Over-all, the workshop was a resounding success. The success stemmed
primarily from a drive to ensure that everyone attending was able to
extract tangible value from the workshop by the end of the day.

A listing of all the aspects that come to mind follows, with a break-down
of the more significant items following that:

## Venue

* Water WTF - Have contingency plans for existential crises
* Lifts - Bottlenecks
* Lunch - Vegans, etc.
* Wifi - What to do when the net is slow / out
* Time spent ensuring that we had a venue delayed the workshop significantly

## Admin

* Reddit fail - Should have used email-based admin tool like Google Groups
* Meetup - Sending messages to attendees / limits
* Delegated responsibilities
* Nailing down the Venue

## People

* Regulating Numbers - Back-Pressure
* Ratios of Volunteers to Attendees
* Categories helped - Admin | Volunteer | Contributor | Participant
* Soliciting Volunteers

## Consolidation

* Maintaining Centralised contacts list
* Have more concrete plans for post-workshop activities

## Future Workshop Ideas

* Intermediate workshop that aims to focus on just one project
* Spit into groups
* Similar to future-proofing software - Very difficult to get right
* School-of-Haskell integration

## Bootstrapping a Dedicated Haskell Meetup

* Workshop is a good opportunity to bootstrap a dedicated meetup

## Other Failures

* Stratification?
* Missing connections between concepts
* External community outreach
* Cross-Platform Midi Players
* Cabal Constraints on Web-Site scaffold
* The "Domain Modelling" aspect was somewhat sidelined
* Not enough focus on types as language for communication
* Compilation errors in the IDE were hard to pinpoint
* Expand-Contract in Material loses position
* Public speaking in a workshop context is very different
  to a lecture context. Why?
* Unknown unknowns emerged very late in the process
* Not initially having copies of static assets on hand

## Other Wins

* Incremental approach to material construction - Map-Reduce
* Taking a firm-stance on the experience level of attendees
* Name-Tags
* Ambitious goal achieved
* Fast-Pace with volunteers to provide hands-on assistance
* Open-Ended questions
* Easy-going approach
* Interactivity
* Material
* Project structure on GitHub facilitating collaboration
* Academic aspects were marginalised for the sake of practicality
* In response to stratification - On demand workflow
* Shotgun approach to concepts - Don't try to build a perfect
  in-order dependency graph. Introduce groups of concepts all
  at once, then provide hands-on-assistance with issues
* Snippet error-checking in the IDE
* Research on running workshops and talking to people with
  previous experience helped catch some mistakes before they
  occurred
* Relaxed approach worked - Didn't try to control every aspect
* Color coding of the material

The first rude shock of the day came just as I was helping to set up
the venue. I received a text saying that the water to the building
would be cut off at 1pm until the end of the day. There was no real
way this could have been predicted, but having a general plan to deal
with such potentially existential crises would have been a good idea.
In the end, the water wasn't a problem as we made sure that everyone
was aware of the situation and advised of the location of the nearest
public toilets, etc.

Another piece of foresight that would have been valuable was that
the internet may not have the bandwidth to support 50 people all
trying to `cabal update`, etc. At the start of the workshop, local
copies of the workshop material and scaffold projects were downloaded
so that people could get a copy if their wifi wasn't performing as
needed. Luckily, for the most part, the wifi held up and we only
had to distribute a couple of thumb-drives around to get the
people who hadn't come prepared set up. This could have gone much
worse if the wifi was flakier.

In order to organise the various aspects of the administrative effort,
I decided to use a dedicated sub-reddit. I hoped that this would
aid in helping to prevent discussion topics from becoming side-tracked
and allow voting to dictate the importance of various issues.
In reality a mailing-list approach such as Google Groups would
have been a much wiser choice. The main issue was that people weren't
notified when new messages were posted to the subreddit. I still
see value in taking the subreddit approach, but wouldn't recommend it
for organising an even with less than about 200 participants.

Making sure that we had the venue available was a fairly involved
process which ended up delaying the workshop somewhat.
There was a lot of back and forth communication between various parties and
little direct communication with the people involved in making the
final call. In retrospect, ensuring that the venue was considered
a hard-dependency for announcing the workshop was a wise decision,
but I would have pressed for firmer answers about the availability
and worried less about offending people.

Regulating attendance involved some consideration, but worked out in the end.
Although I set up the workshop on Meetup.com I wasn't able to set limits on
the attendance. I had to get the creator of the group to change it for me.
It would have been very convenient if this was an option and it seems
like a fairly serious issue with the meetup interface that various
controls are absent from meetup creators settings and only available
to the group admin. Having the participation numbers highly visible was
very useful and allowed us to make the decision not to promote the workshop
outside of Meetup.com as we had originally planned. This avoided having
a large waiting list, or too many attendees for the capacity of the venue.
Another tactic we used in limiting the number of attendees was to heavily
recommend that people with more advanced experience with Haskell consider attending in a volunteer
capacity as opposed to as a regular participant.
This also had the positive effect of boosting numbers of volunteers.

Since the announcement there has been a great deal of interest in follow-up
or ongoing workshops. The idea of running an intermediate-to-advanced
workshop is fairly daunting. The best alternative we have thought of
is to, instead, create a regular dedicated Haskell meetup where people are
free to bring their own ideas forward and work collaboratively if they
wish to do so. This would also alleviate concerns of venue capacity
saturation, as since it would be an ongoing event, there would be
less of a nexus of turnout. One of the original goals of the workshop
was to consolidate the Melbourne Haskell community - this had to be
abandoned somewhat when it was decided that this would only serve
as an introductory workshop. Having a meetup that accommodates
various skill levels and topics would definitely serve as a platform
that would be more amenable to promotion to the wider Melbourne
Haskell community.

It was always a concern of ours that the various previous exposure,
general skill-levels, and disposition of the attendees would heavily stratify
progress through the exercises during the workshop. Our plan was
originally to proceed with the chapters in time-allotments decided in advance.
This is how we approached the workshop for the before-lunch segment, however,
it became apparent that it would not be viable to continue this approach for
the remainder of the afternoon. After a discussion with the volunteers,
Logan suggested that we provide a summary of the remaining material
in a short talk after lunch, then allow participants to continue at their
own pace. This increased stratification, but drastically lessened the
impact that the stratification was having.

Another of the original goals of the workshop was to take a "domain modelling"
driven approach to the exercises. I still believe that this is a noble goal,
however, when addressing all of the concerns that we had, this took a lower
priority and ended up not playing a significant role in the workshop structure.
Compromises like this become accentuated when a deadline approaches and
hard decisions have to be made about priorities. I believe that we
made the right choice by sacrificing this goal, but I would definitely
like to see a workshop structured around this principle some time in
the future.

As far as failings are concerned, that is the meat of it. Here are some of
the aspects of the workshop that went very well...

The material for the workshop was constructed in a shot-gun fashion.
Any and every idea was given a go and included in the material with
the intention that later in the process poor or irrelevant material
would be removed, overlapping material consolidated, and insufficiently
explained material expanded upon. Given that I had no idea if this would
work, it worked remarkably well. Andy Kitchen and myself produced a large
quantity of whatever took our fancy at the time in markdown, and dumped it all into
one large `index.html` through the judicious use of make-files.
Various tools emerged during the process to help aid the construction and
verification of material, such as snippet checkers, pre-processors, and
change-watching scripts that built the content automatically as changes
were made.

The goal of the workshop was fairly ambitious - Introduce a group of
nearly 50 people to a significant chunk of the Haskell language with
the desired outcome being that they would be able to have confidence
in attacking problems using Haskell on their own once the workshop
was over. This made it quite important that we not only covered
a self-consistent subset of the language, but a fair chunk of what
they might come across in real projects. If the workshop had been
run as a "here is the material - go for your life" fashion, then
there would have been a great deal of issues with trying to cover
this much ground. The way we mitigated this was by having a good
ratio of volunteers to attendees. People were heavily encouraged
to ask for help as soon as they had any issues with an exercise
or concept, and this managed to keep, at least from my perspective,
any people from becoming hopelessly confused or frustrated.

A decision I made early on for purely stylistic reasons - the color coding
of the various chapters of the material - ended up being immensely helpful
in gauging the progress of the group, as a simple glance around
the room was enough to spot anyone who was falling behind. I never
would have predicted this and it was just a happy coincidence that it
worked out that way, If I had have predicted the utility of the colors
then I would have ensured that I knew which color corresponded to which
chapter, and would have made the colors unique.

After-workshop beers were much appreciated, as running the workshop the way
we did, while rewarding, was also tiring. I look forward to engaging
with the participants, volunteers, administrators, and sponsors in the future
and if we have increased awareness, proliferation and acceptance of
Haskell in the Melbourne community then I feel that the workshop was a resounding
success :-)
