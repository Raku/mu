# See also: L<http://theory.cs.iitm.ernet.in/~arvindn/pi/> :)
sub JS::Root::pi () is primitive { 3.14159265358979323846264338327950288419716939937510 }

sub JS::Root::atan (Num $y, Num ?$x) is primitive {
  defined($x) ?? $JS::Math.atan2(+$y, +$x) :: $JS::Math.atan(+$y);
}

method sin  (Num $self:) { $JS::Math.sin(+$self)  }
method cos  (Num $self:) { $JS::Math.cos(+$self)  }
method tan  (Num $self:) { $JS::Math.tan(+$self)  }
method asin (Num $self:) { $JS::Math.asin(+$self) }
method acos (Num $self:) { $JS::Math.acos(+$self) }
